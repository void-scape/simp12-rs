#![allow(clippy::too_many_arguments)]

use simp12_rs::{
    assembler,
    control::ControlFlags,
    cpu::{
        self, Acc, Alu, Cpu, Cycles, DeExLatch, ExMemLatch, IfDeLatch, InstrTrace, Mem, Pc,
        PcAdder, PcMux, Stalls,
    },
    param::{B, S, Signal},
};
use std::cell::{Ref, RefMut};

fn main() {
    let mut args = std::env::args();
    let binary = args.next().unwrap();
    let memfile = match args.next() {
        Some(path) => std::fs::read(path).unwrap(),
        None => {
            println!("Usage: {binary} path/to/memFile");
            std::process::exit(1);
        }
    };
    let stats = &assembler::memory_file_statistics(&memfile).unwrap();
    let machine_code = assembler::memory_file(&memfile).unwrap();
    let cpu = Cpu::new(machine_code);

    const HALT: u8 = 0b1111;
    let check_halt = move |latch: Ref<IfDeLatch>,
                           mem: RefMut<Mem>,
                           cycles: Ref<Cycles>,
                           stalls: Ref<Stalls>,
                           trace: Ref<InstrTrace>| {
        if latch.ir == HALT {
            println!("### Execution Time in Cycles ###");
            println!("{}", cycles.0);
            println!("### Stalls Inserted ###");
            println!("{}", stalls.0);
            println!("### Instruction Mix ###");
            println!("Instruction Count: {}", stats.number_of_instructions);
            print!("{}", stats.instr_mix);
            println!("### Instruction Trace ###");
            println!("Total Executed Instructions: {}", trace.count);
            print!("{}", trace.trace);
            println!("### Memory ###");
            println!("{}", mem.pretty_fmt());
            std::process::exit(0);
        }
    };

    loop {
        cpu.run((
            mem,
            deexl,
            execute,
            pcmux,
            pc,
            acc,
            (check_halt, cpu::finish_cycle),
        ));
    }
}

struct Stall;

fn mem(
    mut mem: RefMut<Mem>,
    mut ifdel: B<IfDeLatch>,
    mut deexl: B<DeExLatch>,
    mut stall: S<Stall>,
    exmeml: Ref<ExMemLatch>,
    pc: Ref<Pc>,
    acc: Ref<Acc>,
    mut trace: RefMut<InstrTrace>,
) {
    *stall = Signal::Low;

    let mem_read_de = ifdel.control.mem_read_de();
    let mem_write = exmeml.control.mem_write();

    let mem_read_if = !mem_write && !mem_read_de;

    assert!(!(mem_read_if && mem_read_de));
    assert!(!mem_write || !mem_read_de && !mem_read_if);

    let mem_sel = ifdel.control.mem_read_de();
    let read_addr = if !mem_sel { pc.0 } else { ifdel.mar };

    let word = mem.read(read_addr);
    let control = match word.high_nibble() {
        Alu::ADD | Alu::SUB | Alu::AND | Alu::OR => ControlFlags::MATH,
        Mem::STORE => ControlFlags::STORE,
        Mem::LOAD => ControlFlags::LOAD,
        PcMux::JMP | PcMux::JN | PcMux::JZ => ControlFlags::JMP,
        _ => ControlFlags::NONE,
    };

    if mem_read_if
        && !control.hazard(ifdel.control)
        && !control.hazard(deexl.control)
        && !control.hazard(exmeml.control)
    {
        if ifdel.ir != 0b1111 {
            trace.count += 1;
            trace.trace.push_str(&format!(
                "{}\n",
                assembler::opcode_as_str(word.high_nibble())
            ));
        }

        ifdel.buffered_write(move |ifdel| {
            ifdel.control = control & ControlFlags::DECODE_FLAGS;
            ifdel.ir = word.high_nibble();
            ifdel.mar = word.truncate();
        });
    } else {
        *stall = Signal::High;
        ifdel.buffered_write(move |ifdel| {
            ifdel.control = ControlFlags::NONE;
        });
    }

    if mem_read_de {
        let word = mem.read(read_addr);
        deexl.buffered_write(move |deexl| {
            deexl.mdr = word;
        });
    }

    if mem_write {
        let write_addr = exmeml.mar;
        mem.write(write_addr, acc.0);
    }
}

fn deexl(mut deexl: B<DeExLatch>, ifdel: Ref<IfDeLatch>) {
    let ir = ifdel.ir;
    let mar = ifdel.mar;
    let control = ifdel.control;
    deexl.buffered_write(move |deexl| {
        deexl.ir = ir;
        deexl.mar = mar;
        deexl.control = control & ControlFlags::EXECUTE_FLAGS;
    });
}

fn execute(mut alu: RefMut<Alu>, mut exmeml: B<ExMemLatch>, acc: Ref<Acc>, deexl: Ref<DeExLatch>) {
    let ir = deexl.ir;
    let mar = deexl.mar;
    let mdr = deexl.mdr;
    let control = deexl.control;
    let alu_result = alu.process(acc.0, deexl.mdr, deexl.ir);

    exmeml.buffered_write(move |exmeml| {
        exmeml.ir = ir;
        exmeml.mar = mar;
        exmeml.mdr = mdr;
        exmeml.control = control & ControlFlags::MEM_FLAGS;
        exmeml.alu_result = alu_result;
    });
}

fn pcmux(mut pcmux: RefMut<PcMux>, exmeml: Ref<ExMemLatch>, acc: Ref<Acc>) {
    let ir = exmeml.ir;
    let mar = exmeml.mar;
    let acc = acc.0;
    pcmux.write(ir, acc, mar);
}

fn pc(
    mut pc: B<Pc>,
    mut pcadder: RefMut<PcAdder>,
    mut pcmux: RefMut<PcMux>,
    exmeml: Ref<ExMemLatch>,
    stall: S<Stall>,
    mut stalls: RefMut<Stalls>,
) {
    let pcincr = pcadder.add_pc(pc.0);
    pcmux.pc_incr = pc.0;
    let pcmux_val = pcmux.read();
    let pc_sel = exmeml.control.pc_sel();
    let next_pc = if !pc_sel { pcincr } else { pcmux_val };

    if pc_sel {
        pc.buffered_write(move |pc| {
            pc.0 = next_pc;
        });
    } else if stall.is_low() {
        pc.buffered_write(move |pc| {
            pc.0 = next_pc;
        });
    } else {
        stalls.incr();
    }
}

fn acc(mut acc: B<Acc>, exmeml: Ref<ExMemLatch>) {
    if exmeml.control.acc_write() {
        let alu_result = exmeml.alu_result;
        let mx = exmeml.mdr;
        let select_alu = !exmeml.control.a_sel();
        let new_acc = if select_alu { alu_result } else { mx };
        acc.buffered_write(move |acc| {
            acc.0 = new_acc;
        });
    }
}
