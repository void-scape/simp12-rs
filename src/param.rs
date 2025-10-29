use crate::cpu::Cpu;
use std::cell::{Ref, RefMut};

/// A parameter in a [`System`].
///
/// To borrow a component of the [`Cpu`], use [`C`], and to mutably borrow,
/// use [`CMut`].
///
/// [`System`]: crate::system::System
pub trait SystemParam {
    type Item<'a>;
    fn from_cpu<'a>(cpu: &'a Cpu) -> Self::Item<'a>;
}

/// Read (shared) access to a component in [`Cpu`].
///
/// ## Basic Usage
/// ```
/// # use simp12_rs::cpu;
/// # use simp12_rs::param::*;
/// fn my_system(program_counter: R<cpu::Pc>) {
///     println!("{program_counter:?}");
/// }
/// ```
#[derive(Debug)]
pub struct R<'a, T>(Ref<'a, T>);

impl<'a, T> R<'a, T> {
    pub fn new(v: Ref<'a, T>) -> Self {
        Self(v)
    }
}

impl<'a, T> core::ops::Deref for R<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Write (mutable) access to a component in [`Cpu`].
///
/// ## Basic Usage
/// ```
/// # use simp12_rs::cpu;
/// # use simp12_rs::param::*;
/// fn my_system(mut program_counter: W<cpu::Pc>) {
///     program_counter.0 += 1;
/// }
/// ```
#[derive(Debug)]
pub struct W<'a, T>(RefMut<'a, T>);

impl<'a, T> W<'a, T> {
    pub fn new(v: RefMut<'a, T>) -> Self {
        Self(v)
    }
}

impl<'a, T> core::ops::Deref for W<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, T> core::ops::DerefMut for W<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[macro_export]
macro_rules! impl_system_param {
    ($ty:ty, $field:ident) => {
        impl $crate::param::SystemParam for $crate::param::R<'_, $ty> {
            type Item<'c> = $crate::param::R<'c, $ty>;
            fn from_cpu<'a>(cpu: &'a Cpu) -> Self::Item<'a> {
                $crate::param::R::new(cpu.$field.borrow())
            }
        }

        impl $crate::param::SystemParam for $crate::param::W<'_, $ty> {
            type Item<'c> = $crate::param::W<'c, $ty>;
            fn from_cpu<'a>(cpu: &'a Cpu) -> Self::Item<'a> {
                $crate::param::W::new(cpu.$field.borrow_mut())
            }
        }
    };
}
