use crate::{
    param::W,
    system::SystemSet,
    word::{Word, word},
};
use std::cell::RefCell;

macro_rules! cpu {
    {
        $(#[$attrs:meta])*
        pub struct $ident:ident {
            $($field:ident: $field_ty:tt,)*
        }
    } => {
        $(#[$attrs])*
        pub struct $ident {
            $($field: RefCell<$field_ty>,)*
        }
        $(crate::impl_system_param!($field_ty, $field);)*
    };
}

cpu! {
    /// S12 CPU architecture components.
    ///
    /// ## Basic Usage
    /// ```
    /// # use simp12_rs::cpu::*;
    /// # use simp12_rs::param::*;
    /// fn instruction_fetch(mut mem: W<Mem>, mut pc: W<Pc>, pcmux: R<PcMux>) {
    ///     mem.addr(pc.0);
    ///     pc.0 = pcmux.read();
    /// }
    /// # let cpu = Cpu::default();
    ///
    /// cpu.run(instruction_fetch);
    /// ```
    #[derive(Debug, Default)]
    pub struct Cpu {
        cycles: Cycles,
        //
        memory: Mem,
        pc: Pc,
        pcaddr: PcAdder,
        acc: Acc,
        alu: Alu,
        pcmux: PcMux,
        //
        ifdel: IfDeLatch,
        deexl: DeExLatch,
        exmeml: ExMemLatch,
    }
}

impl Cpu {
    /// Create a [`Cpu`] initialized with `memory`.
    pub fn new(memory: &[Word]) -> Self {
        let mut mem = [word(0); 256];
        assert!(memory.len() <= 256);
        mem[..memory.len()].copy_from_slice(memory);
        Self {
            memory: RefCell::new(Mem {
                memory: mem,
                active_addr: 0,
            }),
            ..Default::default()
        }
    }

    /// Execute a set of systems.
    ///
    /// The systems are executed in the order they appear within the tuple.
    pub fn run<M>(&self, set: impl SystemSet<M>) {
        set.run_set(self);
    }
}

/// Tracks the number of cycles executed by [`Cpu`].
///
/// Use the [`incr_cycles`] system to increment cycles.
#[derive(Debug, Default)]
pub struct Cycles(pub usize);

pub fn incr_cycles(mut cycles: W<Cycles>) {
    cycles.0 += 1;
}

/// 8-bit addressable memory.
#[derive(Debug)]
pub struct Mem {
    memory: [Word; 256],
    active_addr: usize,
}

impl Default for Mem {
    fn default() -> Self {
        Self {
            memory: [Word::default(); 256],
            active_addr: 0,
        }
    }
}

impl Mem {
    pub const LOAD: u8 = 0b0100;
    pub const STORE: u8 = 0b0101;

    /// Set the memories active address to `addr`.
    pub fn addr(&mut self, addr: u8) {
        self.active_addr = addr as usize;
    }

    /// Read a [`Word`] from the active address.
    pub fn read(&self) -> Word {
        self.memory[self.active_addr]
    }

    /// Write `word` to the active address.
    pub fn write(&mut self, word: Word) {
        self.memory[self.active_addr] = word;
    }

    /// Well formatted string for debugging.
    pub fn pretty_fmt(&self) -> String {
        let mut str = String::new();
        for (i, slice) in self.memory.chunks(4).enumerate() {
            str.push_str(&format!("{:#06X}\t", i));
            for word in slice.iter() {
                str.push_str(&format!("{:#08X} ", word.into_inner()));
            }
            str.push('\n');
        }
        str
    }
}

/// Program counter register.
#[derive(Debug, Default)]
pub struct Pc(pub u8);

/// Adds a constant value, intended for the [`Pc`].
#[derive(Debug)]
pub struct PcAdder(u8);

impl Default for PcAdder {
    fn default() -> Self {
        Self(1)
    }
}

impl PcAdder {
    /// Add to `pc` by the value stored in `self`.
    pub fn add_pc(&mut self, pc: u8) -> u8 {
        self.0 + pc
    }
}

/// Accumulator register.
#[derive(Debug, Default)]
pub struct Acc(pub Word);

/// Arithmetic logic unit.
#[derive(Debug, Default)]
pub struct Alu(Word);

impl Alu {
    pub const AND: u8 = 0b1000;
    pub const OR: u8 = 0b1001;
    pub const ADD: u8 = 0b1010;
    pub const SUB: u8 = 0b1011;

    pub fn read(&self) -> Word {
        self.0
    }

    pub fn write(&mut self, acc: Word, mx: Word, func_sel: u8) {
        match func_sel {
            Alu::AND => {
                self.0 = word(acc.into_inner() & mx.into_inner());
            }
            Alu::OR => {
                self.0 = word(acc.into_inner() | mx.into_inner());
            }
            Alu::ADD => {
                self.0 = acc.wrapping_add(mx);
            }
            Alu::SUB => {
                self.0 = acc.wrapping_sub(mx);
            }
            _ => {
                // If the opcode does not correspond to an ALU function, just
                // write 0 to the output.
                self.0 = word(0);
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct IfDeLatch {
    pub ir: u8,
    pub mar: u8,
}

#[derive(Debug, Default)]
pub struct DeExLatch {
    pub ir: u8,
    pub mar: u8,
    pub mdr: Word,
}

#[derive(Debug, Default)]
pub struct ExMemLatch {
    pub ir: u8,
    pub mar: u8,
    pub mdr: Word,
    pub alu_result: Word,
}

/// Multiplexer used to determine what value to set the [`Pc`].
#[derive(Debug)]
pub struct PcMux {
    ir: u8,
    a: Word,
    mar: u8,
    pc_incr: u8,
}

impl Default for PcMux {
    fn default() -> Self {
        Self {
            // Initialize to an `ir` value that will automatically choose `pc_incr`.
            // This way, when the program starts, `PcMux` will correctly advance the
            // `Pc`.
            ir: 0xFF,
            a: word(0),
            mar: 0,
            pc_incr: 0,
        }
    }
}

impl PcMux {
    pub const JMP: u8 = 0b00;
    pub const JN: u8 = 0b01;
    pub const JZ: u8 = 0b10;

    /// Read from the [`PcMux`].
    pub fn read(&self) -> u8 {
        match self.ir {
            Self::JMP => self.mar,
            Self::JN => {
                if self.a.sign_bit() {
                    self.mar
                } else {
                    self.pc_incr
                }
            }
            Self::JZ => {
                if self.a == word(0) {
                    self.mar
                } else {
                    self.pc_incr
                }
            }
            _ => {
                // This is not a jump instruction, so this must return PC + 1.
                self.pc_incr
            }
        }
    }

    /// Write to the [`PcMux`].
    pub fn write(&mut self, ir: u8, a: Word, mar: u8, pc_incr: u8) {
        self.ir = ir;
        self.a = a;
        self.mar = mar;
        self.pc_incr = pc_incr;
    }
}
