use simp12_rs::{
    cpu::{self, Acc, Alu, Cpu, Cycles, DeExLatch, ExMemLatch, IfDeLatch, Mem, Pc, PcAdder, PcMux},
    param::{R, W},
    word::{Word, word},
};

fn instruction_fetch(mut mem: W<Mem>, mut pc: W<Pc>, pcmux: R<PcMux>) {
    let pre_pc = pc.0;
    pc.0 = pcmux.read();
    mem.addr(pc.0);
    let post_pc = pc.0;
    let diff = post_pc as i32 - pre_pc as i32;
    println!("[IF] PC + {diff} = {:#X}", pc.0);
}

fn ifdel(mut latch: W<IfDeLatch>, mem: R<Mem>) {
    let instr = mem.read();
    latch.ir = instr.high_nibble();
    latch.mar = instr.truncate();

    let instr_label = match latch.ir {
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
        _ => unreachable!(),
    };
    println!("[IF] {instr_label}\t:{:#06X}", latch.mar);
}

fn decode(mut mem: W<Mem>, latch: R<IfDeLatch>) {
    mem.addr(latch.mar);
}

fn deexl(mut latch: W<DeExLatch>, ifdel: R<IfDeLatch>, mem: R<Mem>) {
    latch.ir = ifdel.ir;
    latch.mar = ifdel.mar;
    // TODO: Need some way to know if there are concurrent read/writes.
    latch.mdr = mem.read();
    println!("[DE] MDR:{:#08X}", latch.mdr.into_inner());
}

fn execute(mut alu: W<Alu>, deexl: R<DeExLatch>, acc: R<Acc>) {
    let acc = acc.0;
    let mx = deexl.mdr;
    let func_sel = deexl.ir;
    alu.write(acc, mx, func_sel);
}

fn exmeml(mut latch: W<ExMemLatch>, deexl: R<DeExLatch>, alu: R<Alu>) {
    latch.ir = deexl.ir;
    latch.mar = deexl.mar;
    latch.mdr = deexl.mdr;
    latch.alu_result = alu.read();
}

fn memory(
    mut mem: W<Mem>,
    mut acc: W<Acc>,
    mut pcmux: W<PcMux>,
    mut pcaddr: W<PcAdder>,
    latch: R<ExMemLatch>,
    pc: R<Pc>,
) {
    // write all the shit to the pc multiplexer
    pcmux.write(latch.ir, acc.0, latch.mar, pcaddr.add_pc(pc.0));

    mem.addr(latch.mar);
    match latch.ir {
        Mem::LOAD => {
            acc.0 = mem.read();
        }
        Mem::STORE => {
            mem.write(acc.0);
        }
        Alu::AND | Alu::OR | Alu::ADD | Alu::SUB => {
            acc.0 = latch.alu_result;
        }
        _ => {
            // Jump instructions are handled by the mux
        }
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
    cpu.run(|mut mem: W<Mem>| {
        mem.addr(0xFE);
        mem.write(word(a));
        mem.addr(0xFD);
        mem.write(word(b));
    });

    fn simulated_mult(a: Word, b: Word) -> Word {
        let mut result = word(0);
        for _ in 0..b.into_inner() {
            result = result.wrapping_add(a);
        }
        result
    }

    const HALT: u8 = 0b1111;
    let check_halt = move |latch: R<IfDeLatch>, mut mem: W<Mem>, cycles: R<Cycles>| {
        if latch.ir == HALT {
            println!("{}", mem.pretty_fmt());
            println!("Cycles: {}", cycles.0);
            mem.addr(0xFF);
            let result = mem.read();
            // make sure the program did what we want
            assert_eq!(result, simulated_mult(word(a), word(b)));
            std::process::exit(0);
        }
    };

    loop {
        cpu.run((
            (instruction_fetch, ifdel, cpu::incr_cycles),
            (check_halt, decode, deexl, cpu::incr_cycles),
            (execute, exmeml, cpu::incr_cycles),
            (memory, cpu::incr_cycles),
        ));
    }
}
