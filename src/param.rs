use crate::cpu::Cpu;
use std::{
    cell::{Ref, RefCell, RefMut},
    marker::PhantomData,
};

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

impl SystemParam for &Cpu {
    type Item<'a> = &'a Cpu;
    fn from_cpu<'a>(cpu: &'a Cpu) -> Self::Item<'a> {
        cpu
    }
}

/// Shared and deferred mutable access to a component in [`Cpu`].
///
/// All mutations are buffered and applied in the [`finish_cycle`] system.
///
/// [`finish_cycle`]: crate::cpu::finish_cycle
///
/// ## Basic Usage
/// ```
/// # use simp12_rs::cpu;
/// # use simp12_rs::param::*;
/// fn my_system(mut program_counter: B<cpu::Pc>) {
///     program_counter.buffered_write(|pc: &mut cpu::Pc| pc.0 = 69);
///     assert_ne!(program_counter.0, 69);
/// }
/// ```
pub struct B<'a, T> {
    v: Ref<'a, T>,
    writes: &'a RefCell<Vec<Command>>,
}

pub type BufferedWrite<T> = Box<dyn FnMut(&mut T)>;
pub type Command = Box<dyn FnMut(&Cpu)>;

impl<'a, T> B<'a, T> {
    pub fn new(v: Ref<'a, T>, writes: &'a RefCell<Vec<Command>>) -> Self {
        Self { v, writes }
    }

    pub fn buffered_write(&mut self, mut write: impl FnMut(&mut T) + 'static)
    where
        for<'b> RefMut<'b, T>: SystemParam<Item<'b> = RefMut<'b, T>>,
        T: 'static,
    {
        self.writes.borrow_mut().push(Box::new(move |cpu| {
            let mut component = RefMut::<T>::from_cpu(cpu);
            write(&mut *component);
        }));
    }
}

impl<'a, T> core::ops::Deref for B<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.v
    }
}

/// Value in the [`Cpu`] that represents a binary state.
#[derive(Debug, Default, Clone, Copy)]
pub enum Signal {
    High,
    #[default]
    Low,
}

impl Signal {
    pub fn new(is_high: bool) -> Self {
        if is_high { Self::High } else { Self::Low }
    }

    pub fn is_high(&self) -> bool {
        matches!(self, Signal::High)
    }

    pub fn is_low(&self) -> bool {
        matches!(self, Signal::Low)
    }
}

/// Read and write access to a signal.
///
/// Signals can be any type and are automatically registered.
///
/// ## Basic Usage
/// ```
/// # use simp12_rs::param::*;
/// struct MySignal;
/// fn my_system(mut signal: S<MySignal>) {
///     *signal = Signal::High;
/// }
/// ```
#[derive(Debug)]
pub struct S<'a, T>(&'a mut Signal, PhantomData<T>);

impl<'a, T> S<'a, T> {
    pub fn new(s: &'a mut Signal) -> Self {
        Self(s, PhantomData)
    }
}

impl<'a, T> core::ops::Deref for S<'a, T> {
    type Target = Signal;

    #[track_caller]
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T> core::ops::DerefMut for S<'a, T> {
    #[track_caller]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<T> SystemParam for S<'_, T> {
    type Item<'c> = S<'c, T>;
    fn from_cpu<'a>(cpu: &'a Cpu) -> Self::Item<'a> {
        let name = std::any::type_name::<T>();
        let mut signals = cpu.signals.borrow_mut();
        let entry = signals
            .0
            .entry(name)
            .or_insert_with(|| RefCell::new(Signal::Low));
        S::new(unsafe { &mut *entry.as_ptr() })
    }
}

#[macro_export]
macro_rules! impl_system_param {
    ($ty:ty, $field:ident) => {
        impl $crate::param::SystemParam for core::cell::Ref<'_, $ty> {
            type Item<'c> = core::cell::Ref<'c, $ty>;
            fn from_cpu<'a>(cpu: &'a Cpu) -> Self::Item<'a> {
                cpu.$field.borrow()
            }
        }

        impl $crate::param::SystemParam for core::cell::RefMut<'_, $ty> {
            type Item<'c> = core::cell::RefMut<'c, $ty>;
            fn from_cpu<'a>(cpu: &'a Cpu) -> Self::Item<'a> {
                cpu.$field.borrow_mut()
            }
        }

        impl $crate::param::SystemParam for $crate::param::B<'_, $ty> {
            type Item<'c> = $crate::param::B<'c, $ty>;
            fn from_cpu<'a>(cpu: &'a Cpu) -> Self::Item<'a> {
                $crate::param::B::new(cpu.$field.borrow(), &cpu.commands)
            }
        }
    };
}
