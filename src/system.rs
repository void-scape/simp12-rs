use crate::{cpu::Cpu, param::SystemParam};
use std::marker::PhantomData;

/// Run a function that borrows and mutably borrows components of [`Cpu`] via
/// dependency injection.
///
/// A function is considered a [`System`] if all of its parameters implement
/// [`SystemParam`]. Use [`IntoSystem`] to turn a function into a [`System`].
pub trait System {
    fn run(&mut self, cpu: &Cpu);
}

/// A recursive tuple of [`System`]s.
///
/// ## Example
/// ```
/// # use simp12_rs::cpu::*;
/// # use simp12_rs::param::*;
/// # let cpu = Cpu::default();
/// # fn first_system(_: B<Pc>) {}
/// # fn second_system(_: B<Pc>) {}
/// # fn third_system(_: B<Pc>) {}
/// # fn fourth_system(_: B<Pc>) {}
/// cpu.run(
///     (
///         first_system,
///         second_system,
///         (
///             third_system,
///             fourth_system,
///         ),
///     ),
/// );
/// ```
pub trait SystemSet<M> {
    fn run_set(self, cpu: &Cpu);
}

impl<F, M> SystemSet<M> for F
where
    F: IntoSystem<M>,
{
    fn run_set(self, cpu: &Cpu) {
        self.into_system().run(cpu);
    }
}

/// Converts a function into a [`System`].
pub trait IntoSystem<M>: Sized {
    type System: System;
    fn into_system(self) -> Self::System;
}

/// Wraps a normal function and implements [`System`].
///
/// This is required to implement [`System`] because without the `P` generic,
/// nothing in the impl block can constrain the parameter generics.
pub struct FnSystem<F, P>(F, PhantomData<P>);

macro_rules! impl_system {
    ($($param:ident),*) => {
        #[allow(unused_parens)]
        impl<FS, $($param),*> System for FnSystem<FS, ($($param),*)>
        where
            FS: FnMut($(<$param as SystemParam>::Item<'_>),*),
            $($param: SystemParam),*
        {
            fn run(&mut self, cpu: &Cpu) {
                (self.0)($($param::from_cpu(cpu)),*);
            }
        }
    };
}

macro_rules! impl_system_set {
    ($(($set:ident, $marker:ident)),*) => {
        #[allow(unused_parens)]
        #[expect(non_snake_case)]
        impl<$($set, $marker),*> SystemSet<($($marker),*)> for ($($set),*)
        where
            $($set: SystemSet<$marker>),*
        {
            fn run_set(self, cpu: &Cpu) {
                let ($($set),*) = self;
                $(($set).run_set(cpu);)*
            }
        }
    };
}

macro_rules! impl_into_system {
    ($($param:ident),*) => {
        #[allow(unused_parens)]
        impl<FS, $($param: SystemParam),*> IntoSystem<($($param),*)> for FS
        where
            for<'a> FS:
                FnMut($($param),*) +
                FnMut($(<$param as SystemParam>::Item<'_>),*),
        {
            type System = FnSystem<FS, ($($param),*)>;
            fn into_system(self) -> Self::System {
                FnSystem(self, PhantomData)
            }
        }
    };
}

macro_rules! impl_system_traits {
    ($($param:ident),*) => {
        impl_system!($($param),*);
        impl_into_system!($($param),*);
    };
}

impl_system_traits!(A);
impl_system_traits!(A, B);
impl_system_traits!(A, B, C);
impl_system_traits!(A, B, C, D);
impl_system_traits!(A, B, C, D, E);
impl_system_traits!(A, B, C, D, E, F);
impl_system_traits!(A, B, C, D, E, F, G);
impl_system_traits!(A, B, C, D, E, F, G, H);
impl_system_traits!(A, B, C, D, E, F, G, H, I);
impl_system_traits!(A, B, C, D, E, F, G, H, I, J);
impl_system_traits!(A, B, C, D, E, F, G, H, I, J, K);

impl_system_set!((A0, A1), (B0, B1));
impl_system_set!((A0, A1), (B0, B1), (C0, C1));
impl_system_set!((A0, A1), (B0, B1), (C0, C1), (D0, D1));
impl_system_set!((A0, A1), (B0, B1), (C0, C1), (D0, D1), (E0, E1));
impl_system_set!((A0, A1), (B0, B1), (C0, C1), (D0, D1), (E0, E1), (F0, F1));
impl_system_set!(
    (A0, A1),
    (B0, B1),
    (C0, C1),
    (D0, D1),
    (E0, E1),
    (F0, F1),
    (G0, G1)
);
impl_system_set!(
    (A0, A1),
    (B0, B1),
    (C0, C1),
    (D0, D1),
    (E0, E1),
    (F0, F1),
    (G0, G1),
    (H0, H1)
);

#[cfg(test)]
mod test {
    use crate::cpu::{Cpu, Pc};
    use std::cell::{Ref, RefMut};

    fn inc(mut pc: RefMut<Pc>) {
        pc.0 += 1;
    }

    #[test]
    fn system() {
        let cpu = Cpu::default();
        cpu.run((inc, |pc: Ref<Pc>| {
            assert_eq!(pc.0, 1);
        }));

        let cpu = Cpu::default();
        cpu.run((inc, inc, |pc: Ref<Pc>| {
            assert_eq!(pc.0, 2);
        }));
    }
}
