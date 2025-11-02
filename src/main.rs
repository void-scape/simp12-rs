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
    let mut memfile = match args.next() {
        Some(path) => std::fs::read_to_string(path).unwrap(),
        None => {
            println!("Usage: {binary} path/to/memFile [-c]");
            std::process::exit(1);
        }
    };

    match args.next().as_deref() {
        Some("-c") => {
            memfile = assembler::assemble(&memfile).unwrap();
        }
        Some(_) => {
            println!("Usage: {binary} path/to/memFile [-c]");
            std::process::exit(1);
        }
        None => {}
    }

    let stats = &assembler::memory_file_statistics(memfile.as_bytes()).unwrap();
    let machine_code = assembler::memory_file(memfile.as_bytes()).unwrap();
    let cpu = Cpu::new(machine_code);

    const HALT: u8 = 0b1111;
    let check_halt = move |latch: Ref<ExMemLatch>,
                           mem: RefMut<Mem>,
                           cycles: Ref<Cycles>,
                           stalls: Ref<Stalls>,
                           trace: Ref<InstrTrace>| {
        if latch.ir == HALT {
            println!("### Execution Time ###");
            println!("Pipelined: {} cycles", cycles.0);
            println!("Non-pipelined: {} cycles", trace.count * 4);
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
    mut exmeml: B<ExMemLatch>,
    pc: Ref<Pc>,
    acc: Ref<Acc>,
    mut trace: RefMut<InstrTrace>,
) {
    *stall = Signal::Low;

    let mem_read_de = ifdel.control.mem_read_de();
    let mem_read_ex = deexl.control.mem_read_ex();
    let mem_write = exmeml.control.mem_write();
    let mem_read_if = !mem_write && !mem_read_de && !mem_read_ex;

    assert!(!(mem_read_if && mem_read_de));
    assert!(!(mem_read_de && mem_read_ex));
    assert!(!(mem_read_if && mem_read_ex));
    assert!(!mem_write || !mem_read_if && !mem_read_de && !mem_read_ex);

    let mar_addr = if mem_write { exmeml.mar } else { ifdel.mar };
    let direct_indirect_addr = if exmeml.control.indirect() && deexl.ir == Mem::STOREI {
        exmeml.mddr.truncate()
    } else if deexl.control.indirect() && deexl.ir == Mem::LOADI {
        deexl.mdr.truncate()
    } else {
        mar_addr
    };

    let addr = if mem_read_if {
        pc.0
    } else {
        direct_indirect_addr
    };

    if mem_write {
        mem.write(addr, acc.0);
    } else if mem_read_ex {
        let word = mem.read(addr);
        exmeml.buffered_write(move |exmeml| {
            exmeml.mddr = word;
        });
    } else if mem_read_de {
        let word = mem.read(addr);
        deexl.buffered_write(move |deexl| {
            deexl.mdr = word;
        });
    } else if mem_read_if {
        let word = mem.read(addr);
        let control = match word.high_nibble() {
            Alu::ADD | Alu::SUB | Alu::AND | Alu::OR => ControlFlags::MATH,
            Mem::LOAD => ControlFlags::LOAD,
            Mem::STORE => ControlFlags::STORE,
            Mem::LOADI => ControlFlags::LOADI,
            Mem::STOREI => ControlFlags::STOREI,
            PcMux::JMP | PcMux::JN | PcMux::JZ => ControlFlags::JMP,
            _ => ControlFlags::NONE,
        };

        if !control.hazard(ifdel.control)
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
            // Don't stall at the end of the function
            return;
        }
    }

    *stall = Signal::High;
    ifdel.buffered_write(move |ifdel| {
        ifdel.control = ControlFlags::NONE;
    });
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

    if pc_sel || stall.is_low() {
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
        let mmx = exmeml.mddr;
        let a_sel = exmeml.control.a_sel();

        let data = if exmeml.control.indirect() { mmx } else { mx };
        let new_acc = if a_sel { data } else { alu_result };
        acc.buffered_write(move |acc| {
            acc.0 = new_acc;
        });
    }
}
