#![recursion_limit="2000"]

use std::{marker::PhantomData};

trait Nat {}

struct Z;

struct S<N: Nat>(PhantomData<N>);

impl Nat for Z {}
impl<N> Nat for S<N> where N: Nat {}

trait Term {}

struct Var<N: Nat>(PhantomData<N>);
struct App<F: Term, L: Term>(PhantomData<F>, PhantomData<L>);
struct Abs<E: Term>(PhantomData<E>);

impl<N> Term for Var<N> where N: Nat {}
impl<F,L> Term for App<F,L> where F: Term, L: Term {}
impl<E> Term for Abs<E> where E: Term {}

trait Bool{}
struct True; struct False;

impl Bool for True {}
impl Bool for False {}

trait IfT {
    type Output: Term;
}

impl<A: Term, B: Term> IfT for (True, A, B) {
    type Output = A;
}

impl<A: Term, B: Term> IfT for (False, A, B) {
    type Output = B;
}


trait LessT {
    type Output: Bool;
}

impl<N: Nat> LessT for (Z, S<N>) {
    type Output = True;
}

impl LessT for (Z, Z) {
    type Output = False;
}

impl<N: Nat> LessT for (S<N>, Z) {
    type Output = False;
}

impl<N: Nat, M: Nat> LessT for (S<N>, S<M>) 
where (N, M) : LessT
{
    type Output = <(N, M) as LessT>::Output;
}

trait Shift {
    type Output: Term;
}
//
impl<C: Nat, I: Nat, N: Nat> Shift for (C, I, Var<N>) {
    type Output = <(<(N,C) as LessT>::Output, Var<N>, Var<S<N>>) as IfT>::Output ;
}

impl<C: Nat, I: Nat, T: Term> Shift for (C, I, Abs<T>) {
    type Output = Abs< <(C, I, T) as Shift>::Output >;
}

impl<C: Nat, I: Nat, F: Term, L: Term> Shift for (C, I, App<F, L>) {
    type Output = App< 
        <(C, I, F) as Shift>::Output, 
        <(C, I, L) as Shift>::Output 
    >;
}

trait And {
    type Output: Bool;
}

impl And for (True, False) {type Output = False;}
impl And for (True, True) {type Output = True;}
impl And for (False, False) {type Output = False;}
impl And for (False, True) {type Output = False;}

trait Pred {
    type Output: Nat;
}

impl Pred for Z {
    type Output = Z;
}

impl<N: Nat> Pred for S<N> {
    type Output = N;
}

trait Sub {
    type Output: Nat;
}

impl<N: Nat> Sub for (Z, N) {
    type Output = Z;
}

impl<N: Nat> Sub for (S<N>, Z) {
    type Output = S<N>;
}

impl<N: Nat, M:Nat> Sub for (S<N>, S<M>) {
    type Output = <(N, M) as Sub>::Output;
}


trait Unshift {
    type Output: Term;
}
//
impl<C: Nat, I: Nat, N: Nat> Unshift for (C, I, Var<N>) {
    type Output = <(<(N,C) as LessT>::Output, Var<N>, Var<<N as Pred>::Output>) as IfT>::Output ;
}

impl<C: Nat, I: Nat, T: Term> Unshift for (C, I, Abs<T>) {
    type Output = Abs< <(C, I, T) as Unshift>::Output >;
}

impl<C: Nat, I: Nat, F: Term, L: Term> Unshift for (C, I, App<F, L>) {
    type Output = App< 
        <(C, I, F) as Unshift>::Output, 
        <(C, I, L) as Unshift>::Output 
    >;
}

trait Substitution {
    type Output: Term;
}

impl<N: Nat, T: Term, M: Nat> Substitution for (Var<N>, T, M) {
    type Output = <(
         <(<(<(N, M) as Sub>::Output, S<Z>) as LessT>::Output,
         <(<(M, N) as Sub>::Output, S<Z>) as LessT>::Output) as And>::Output,
         T, Var<N>
    ) as IfT>::Output;
}

impl<F: Nat, L:Term, T: Term, M: Nat> Substitution for (App<F, L>, T, M) {
    type Output = App<
        <(F, T, M) as Substitution>::Output,
        <(L, T, M) as Substitution>::Output
    >;
}

impl<E: Nat, T: Term, M: Nat> Substitution for (Abs<E>, T, M) {
    type Output = Abs<<(
        E,
        <(Z, S<Z>, T) as Shift>::Output,
        S<M>
    ) as Substitution>::Output>;
}

trait Beta {
    type Output: Term;
}

impl<T1: Term, T2: Term> Beta for App<Abs<T1>, T2> {
    type Output = <( Z, S<Z>, <(
        T1,
        <(Z, S<Z>, T2) as Shift>::Output,
        Z
    )as Substitution>::Output)as Unshift>::Output;
}

impl<T1: Term, T11: Term, T2: Term> Beta for App<App<T1,T11>, T2> {
    type Output = App<<App<T1, T11> as Beta>::Output, <T2 as Beta>::Output>;
}

impl<T1: Nat, T2: Term> Beta for App<Var<T1>, T2> {
    type Output = App<<Var<T1> as Beta>::Output, <T2 as Beta>::Output>;
}

impl<T1: Term> Beta for Abs<T1> {
    type Output = Abs<<T1 as Beta>::Output>;
}

impl<T1: Nat> Beta for Var<T1> {
    type Output = Var<T1>;
}

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}


fn main() {
    let _x: Abs<Abs<App<App<Var<S<Z>>, Var<Z>>, Abs<Abs<Var<Z>>> >>> = Abs(PhantomData);
    print_type_of(_x);
    println!("");
}
