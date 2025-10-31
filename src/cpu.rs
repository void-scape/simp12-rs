use crate::{
    assembler::MachineCode,
    control::ControlFlags,
    param::{Command, Signal},
    system::SystemSet,
    word::{Word, word},
};
use std::{cell::RefCell, collections::HashMap};

macro_rules! cpu {
    {
        $(#[$attrs:meta])*
        pub struct $ident:ident {
            $($field:ident: $field_ty:ty,)*
        }
    } => {
        $(#[$attrs])*
        pub struct $ident {
            $(pub $field: RefCell<$field_ty>,)*
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
    /// # use simp12_rs::word::*;
    /// # use core::cell::{RefMut, Ref};
    /// fn instruction_fetch(mut mem: RefMut<Mem>, mut pc: RefMut<Pc>, pcmux: Ref<PcMux>) {
    ///     mem.write(pc.0, word(42));
    ///     pc.0 = pcmux.read();
    /// }
    /// # let cpu = Cpu::default();
    ///
    /// cpu.run(instruction_fetch);
    /// ```
    #[derive(Default)]
    pub struct Cpu {
        cycles: Cycles,
        //
        signals: Signals,
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
        //
        commands: Vec<Command>,
    }
}

impl Cpu {
    /// Create a [`Cpu`] initialized with [`MachineCode`].
    pub fn new(machine_code: MachineCode) -> Self {
        Self {
            memory: RefCell::new(Mem(machine_code.program)),
            pc: RefCell::new(Pc(machine_code.pc)),
            acc: RefCell::new(Acc(machine_code.acc)),
            ..Default::default()
        }
    }

    /// Create a [`Cpu`] initialized with `memory`.
    pub fn from_memory(memory: &[Word]) -> Self {
        let mut mem = [word(0); 256];
        assert!(memory.len() <= 256);
        mem[..memory.len()].copy_from_slice(memory);
        Self {
            memory: RefCell::new(Mem(mem)),
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

impl Cycles {
    pub fn incr(&mut self) {
        self.0 += 1;
    }
}

pub fn finish_cycle(cpu: &Cpu) {
    cpu.cycles.borrow_mut().incr();
    for mut command in cpu.commands.borrow_mut().drain(..) {
        command(cpu);
    }
}

/// Dynamic collection of [`Signal`]s associated with a type name.
#[derive(Debug, Default)]
pub struct Signals(pub HashMap<&'static str, RefCell<Signal>>);

/// 8-bit addressable memory.
#[derive(Debug)]
pub struct Mem([Word; 256]);

impl Default for Mem {
    fn default() -> Self {
        Self([Word::default(); 256])
    }
}

impl Mem {
    pub const LOAD: u8 = 0b0100;
    pub const STORE: u8 = 0b0101;

    /// Read a [`Word`] from `addr`.
    pub fn read(&mut self, addr: u8) -> Word {
        self.0[addr as usize]
    }

    /// Write `word` to `addr`.
    pub fn write(&mut self, addr: u8, word: Word) {
        self.0[addr as usize] = word;
    }

    /// Well formatted string for debugging.
    pub fn pretty_fmt(&self) -> String {
        let mut str = String::new();
        for (i, slice) in self.0.chunks(4).enumerate() {
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
pub struct Alu;

impl Alu {
    pub const AND: u8 = 0b1000;
    pub const OR: u8 = 0b1001;
    pub const ADD: u8 = 0b1010;
    pub const SUB: u8 = 0b1011;

    pub fn process(&mut self, acc: Word, mx: Word, func_sel: u8) -> Word {
        match func_sel {
            Alu::AND => word(acc.into_inner() & mx.into_inner()),
            Alu::OR => word(acc.into_inner() | mx.into_inner()),
            Alu::ADD => acc.wrapping_add(mx),
            Alu::SUB => acc.wrapping_sub(mx),
            _ => {
                // If the opcode does not correspond to an ALU function, just
                // write 0 to the output.
                word(0)
            }
        }
    }
}

/// Initialization value for latch IRs that does not correspond to a valid opcode.
/// This prevents the decode, execute, and memory stages from stalling due to zero
/// being the JMP opcode.
const INIT_IR: u8 = 0b1101;

#[derive(Debug)]
pub struct IfDeLatch {
    pub ir: u8,
    pub mar: u8,
    pub control: ControlFlags,
}

impl Default for IfDeLatch {
    fn default() -> Self {
        Self {
            ir: INIT_IR,
            mar: 0,
            control: ControlFlags::NONE,
        }
    }
}

#[derive(Debug)]
pub struct DeExLatch {
    pub ir: u8,
    pub mar: u8,
    pub mdr: Word,
    pub control: ControlFlags,
}

impl Default for DeExLatch {
    fn default() -> Self {
        Self {
            ir: INIT_IR,
            mar: 0,
            mdr: word(0),
            control: ControlFlags::NONE,
        }
    }
}

#[derive(Debug)]
pub struct ExMemLatch {
    pub ir: u8,
    pub mar: u8,
    pub mdr: Word,
    pub alu_result: Word,
    pub control: ControlFlags,
}

impl Default for ExMemLatch {
    fn default() -> Self {
        Self {
            ir: INIT_IR,
            mar: 0,
            mdr: word(0),
            alu_result: word(0),
            control: ControlFlags::NONE,
        }
    }
}

/// Multiplexer used to determine what value to set the [`Pc`].
#[derive(Debug)]
pub struct PcMux {
    pub pc_incr: u8,
    ir: u8,
    a: Word,
    mar: u8,
}

impl Default for PcMux {
    fn default() -> Self {
        Self {
            pc_incr: 0,
            // Initialize to an `ir` value that will automatically choose `pc_incr`.
            // This way, when the program starts, `PcMux` will correctly advance the
            // `Pc`.
            ir: 0xFF,
            a: word(0),
            mar: 0,
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
    pub fn write(&mut self, ir: u8, a: Word, mar: u8) {
        self.ir = ir;
        self.a = a;
        self.mar = mar;
    }
}
