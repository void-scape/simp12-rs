/// Convenience wrapper for [`Word::new`].
#[track_caller]
pub const fn word(word: u16) -> Word {
    Word::new(word)
}

/// The S12 ISA natural unit of data.
///
/// Composed of `12` bits.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Word(u16);

impl Word {
    const MASK: u16 = 0xFFF;
    const POWER_OF_TWO: u16 = (1 << 12);

    /// Create a new [`Word`] from `bits`.
    ///
    /// Values are truncated to `12` bits.
    #[track_caller]
    pub const fn new(bits: u16) -> Self {
        Self(bits & Self::MASK)
    }

    /// Return the underlying representation of this [`Word`].
    pub const fn into_inner(self) -> u16 {
        debug_assert!(self.0 <= Self::MASK);
        self.0
    }

    /// Return the bottom `8` bits of this [`Word`].
    pub const fn truncate(self) -> u8 {
        debug_assert!(self.0 <= Self::MASK);
        (self.0 & 0xFF) as u8
    }

    /// Return the top `4` bits of this [`Word`].
    pub const fn high_nibble(self) -> u8 {
        debug_assert!(self.0 <= Self::MASK);
        ((self.0 >> 8) & 0xF) as u8
    }

    /// Wrapping (modular) addition. Computes self + rhs, wrapping around at the
    /// boundary of the type.
    #[track_caller]
    pub const fn wrapping_add(self, rhs: Self) -> Self {
        let lhs = self.0;
        let rhs = rhs.0;
        debug_assert!(lhs <= Self::MASK);
        debug_assert!(rhs <= Self::MASK);
        let overflowing_result = lhs + rhs;
        if overflowing_result >= Self::POWER_OF_TWO {
            // https://en.wikipedia.org/wiki/Integer_overflow#Origin
            Self(overflowing_result - Self::POWER_OF_TWO)
        } else {
            // make the common case fast!
            Self(overflowing_result)
        }
    }

    /// Wrapping (modular) subtraction. Computes self - rhs, wrapping around at
    /// the boundary of the type.
    #[track_caller]
    pub const fn wrapping_sub(self, rhs: Self) -> Self {
        // https://en.wikipedia.org/wiki/Two%27s_complement#Subtraction_from_2N
        let tc_rhs = (Self::POWER_OF_TWO - rhs.0) & Self::MASK;
        self.wrapping_add(Self(tc_rhs))
    }

    /// `true` if the sign bit is set, `false` otherwise.
    pub const fn sign_bit(self) -> bool {
        self.0 & (1 << 11) > 0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    impl Word {
        const MAX: Self = Self(Self::MASK);
    }

    #[test]
    fn word_arithmetic() {
        // Word cannot be more than 3 bytes
        assert_eq!(Word::MASK.count_ones(), 12);
        assert_eq!(word(0xFFF), word(0xFFFF));
        assert_eq!(word(0x329), word(0xF329));

        // components
        assert_eq!(word(0x329).truncate(), 0x29);
        assert_eq!(word(0x329).high_nibble(), 0x3);

        // wrapping addition
        assert_eq!(word(5).wrapping_add(word(3)), word(8));
        assert_eq!(word(0xFFF).wrapping_add(word(1)), word(1 - 1));
        assert_eq!(word(0xFFF).wrapping_add(word(0xFF)), word(0xFF - 1));
        assert_eq!(u32::MAX.wrapping_add(u32::MAX), u32::MAX - 1);
        assert_eq!(Word::MAX.wrapping_add(Word::MAX), Word(Word::MAX.0 - 1));

        // twos complement subtraction
        assert_eq!(word(5).wrapping_sub(word(3)), word(2));
        assert_eq!(Word::MAX.wrapping_sub(word(1)), word(Word::MAX.0 - 1));
        assert_eq!(Word::MAX.wrapping_sub(Word::MAX), word(0));
        assert_eq!(word(0).wrapping_sub(word(1)), Word::MAX);
        assert_eq!(word(1).wrapping_sub(word(2)), Word::MAX);

        // sign bit
        assert!(Word::MAX.sign_bit());
        assert!(!word(12).sign_bit());
    }
}
