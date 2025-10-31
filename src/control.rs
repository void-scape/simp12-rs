// Who needs dependencies ¯\_(ツ)_/¯
macro_rules! bitflags {
    {
        $(#[$attrs:meta])*
        pub struct $ident:ident($repr:ty) {
            $($flag:ident($flagfn:ident),)*
        }
    } => {
        $(#[$attrs])*
        pub struct $ident($repr);
        impl $ident {
            bitflags!(0, $(($flag, $flagfn),)*);
        }
    };
    ($index:expr, ($flag:ident, $flagfn:ident), $(($rest:ident, $restfn:ident),)*) => {
        pub const $flag: Self = Self(1 << $index);
        pub fn $flagfn(self) -> bool {
            (self & Self::$flag).0 != 0
        }
        bitflags!($index + 1, $(($rest, $restfn),)*);
    };
    ($index:expr,) => {};
}

bitflags! {
    /// Control signals that propagate through each stage of the pipeline.
    ///
    /// Control hazards are computed with [`ControlFlags::hazard`].
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct ControlFlags(u16) {
        MEM_READ_IF(mem_read_if),
        MEM_READ_DE(mem_read_de),
        MEM_WRITE(mem_write),
        ACC_WRITE(acc_write),
        A_SEL(a_sel),
        PC_SEL(pc_sel),
    }
}

impl ControlFlags {
    pub const NONE: Self = Self(0);
    pub const ALL: Self = Self::MATH.or(Self::LOAD).or(Self::STORE).or(Self::JMP);

    pub const MATH: Self = Self::MEM_READ_IF.or(Self::MEM_READ_DE).or(Self::ACC_WRITE);

    pub const LOAD: Self = Self::MEM_READ_IF
        .or(Self::MEM_READ_DE)
        .or(Self::A_SEL)
        .or(Self::ACC_WRITE);
    pub const STORE: Self = Self::MEM_READ_IF.or(Self::MEM_WRITE);

    pub const JMP: Self = Self::MEM_READ_IF.or(Self::PC_SEL);

    pub const DECODE_FLAGS: Self = Self::MEM_READ_IF.not().and(Self::ALL);
    pub const EXECUTE_FLAGS: Self = (Self::MEM_READ_IF.or(Self::MEM_READ_DE))
        .not()
        .and(Self::ALL);
    pub const MEM_FLAGS: Self = Self::EXECUTE_FLAGS;

    pub const fn or(self, rhs: Self) -> Self {
        Self(self.0 | rhs.0)
    }

    pub const fn and(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }

    pub const fn not(self) -> Self {
        Self(!self.0)
    }

    // ## CONTROL HAZARDS ##
    //
    //                 MEM_READ_IF    MEM_READ_DE    MEM_WRITE
    // MEM_READ_IF         11              11           11
    // MEM_READ_DE         11              11           11
    // MEM_WRITE           11              11           11
    pub fn hazard(self, rhs: Self) -> bool {
        let is_jmp = rhs.pc_sel();

        let r1 = self.mem_read_if() && rhs.mem_read_if()
            || self.mem_read_if() && rhs.mem_read_de()
            || self.mem_read_if() && rhs.mem_write();

        let r2 = self.mem_read_de() && rhs.mem_read_if()
            || self.mem_read_de() && rhs.mem_read_de()
            || self.mem_read_de() && rhs.mem_write();

        let r3 = self.mem_write() && rhs.mem_read_if()
            || self.mem_write() && rhs.mem_read_de()
            || self.mem_write() && rhs.mem_write();

        is_jmp || r1 || r2 || r3
    }
}

impl core::ops::BitOr for ControlFlags {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl core::ops::BitAnd for ControlFlags {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}
