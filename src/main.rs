use simp12_rs::{
    control::ControlFlags,
    cpu::{self, Acc, Alu, Cpu, Cycles, DeExLatch, ExMemLatch, IfDeLatch, Mem, Pc, PcAdder, PcMux},
    param::{B, S, Signal},
    word::{Word, word},
};
use std::cell::{Ref, RefMut};

struct Stall;

fn mem(
    mut mem: RefMut<Mem>,
    mut ifdel: B<IfDeLatch>,
    mut deexl: B<DeExLatch>,
    mut stall: S<Stall>,
    exmeml: Ref<ExMemLatch>,
    pc: Ref<Pc>,
    acc: Ref<Acc>,
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

fn debug(
    pc: Ref<Pc>,
    acc: Ref<Acc>,
    ifdel: Ref<IfDeLatch>,
    deexl: Ref<DeExLatch>,
    exmeml: Ref<ExMemLatch>,
    mut mem: RefMut<Mem>,
) {
    println!("[PC] {:#08X}", pc.0);
    println!("[AC] {:#08X}", acc.0.into_inner());
    debug_ir("IF", mem.read(pc.0).high_nibble(), false);

    if ifdel.control != ControlFlags::NONE {
        debug_ir("IF/DE", ifdel.ir, ifdel.control == ControlFlags::NONE);
        println!("{:#?}", &*ifdel);
    }
    if deexl.control != ControlFlags::NONE {
        debug_ir("DE/EX", deexl.ir, deexl.control == ControlFlags::NONE);
        println!("{:#?}", &*deexl);
    }
    if exmeml.control != ControlFlags::NONE {
        debug_ir("EX/MEM", exmeml.ir, exmeml.control == ControlFlags::NONE);
        println!("{:#?}", &*exmeml);
    }
    println!();
}

fn debug_ir(stage: &'static str, ir: u8, stall: bool) {
    if stall {
        println!("[{stage}] STALL");
    } else {
        let instr_label = match ir {
            PcMux::JMP => "JMP",
            PcMux::JN => "JN",
            PcMux::JZ => "JZ",
            Alu::OR => "OR",
            Alu::AND => "AND",
            Alu::ADD => "ADD",
            Alu::SUB => "SUB",
            Mem::LOAD => "LOAD",
            Mem::STORE => "STORE",
            0b1111 => "HALT",
            _ => "INVALID",
        };
        println!("[{stage}] {instr_label}");
    }
}

// Pierre's S12 memFile: https://github.com/void-scape/simp12/blob/main/bench/mult-2int-mem.memFile
//
// This program takes 169 cycles to multiply 4 and 3 WITHOUT pipelining. I think
// with reasonable pipelineing this can be brought down to around 40-45.
const MULT_2INT: &[Word] = &[
    word(0b000000000001),
    word(0b010011100010),
    word(0b101111100010),
    word(0b010111111111),
    word(0b010011111110),
    word(0b001000001101),
    word(0b010011111111),
    word(0b101011111101),
    word(0b010111111111),
    word(0b010011111110),
    word(0b101100000000),
    word(0b010111111110),
    word(0b000000000100),
    word(0b111100000000),
];

fn main() {
    let cpu = Cpu::new(MULT_2INT);
    // init both integers
    let a = 4;
    let b = 3;
    cpu.memory.borrow_mut().write(0xFE, word(a));
    cpu.memory.borrow_mut().write(0xFD, word(b));

    fn simulated_mult(a: Word, b: Word) -> Word {
        let mut result = word(0);
        for _ in 0..b.into_inner() {
            result = result.wrapping_add(a);
        }
        result
    }

    const HALT: u8 = 0b1111;
    let check_halt = move |latch: Ref<IfDeLatch>, mut mem: RefMut<Mem>, cycles: Ref<Cycles>| {
        if latch.ir == HALT {
            println!("{}", mem.pretty_fmt());
            println!("Cycles: {}", cycles.0);
            let result = mem.read(0xFF);
            // make sure the program did what we want
            assert_eq!(result, simulated_mult(word(a), word(b)));
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
            (check_halt, debug, cpu::finish_cycle),
        ));
    }
}
