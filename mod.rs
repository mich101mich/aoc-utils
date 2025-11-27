#![allow(unused)]
#![allow(clippy::wrong_self_convention)]

pub use std::cmp::Ordering;
pub use std::collections::{
    BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, VecDeque, hash_map::Entry,
};
pub use std::convert::{TryFrom, TryInto};
pub use std::io::{BufRead, BufReader, BufWriter, Read, Write};
pub use std::str::FromStr;

pub use cgmath::{prelude::*, vec2 as p2, vec3 as p3};
pub use rand::prelude::*;
pub use rayon::prelude::*;
pub use regex::Regex;
pub use sscanf::{FromScanf, RegexRepresentation, sscanf};

mod collection;
mod dir;
mod grid;
mod int_code;
mod iter;
mod neighbors;
mod path;
mod point;

pub use collection::*;
pub use dir::*;
pub use grid::*;
pub use int_code::*;
pub use iter::*;
pub use neighbors::*;
pub use path::*;
pub use point::*;

use std::cmp::Eq;
use std::hash::Hash;
use std::ops::*;

macro_rules! pv {
    ($var: expr) => {
        println!("{}: {:?}", stringify!($var), $var)
    };
    ($start: expr, $($var: expr),+) => {
        let mut s = format!("{}: {:?}", stringify!($start), $start);
        $(
            s += &format!(",  {}: {:?}", stringify!($var), $var);
        )*
        println!("{}", s);
    };
}

macro_rules! print_arr {
    ($var: ident) => {
        print!("{}: ", stringify!($var));
        for v in $var.iter() {
            print!("{}", v);
        }
        println!();
    };
}

pub fn copy(out: String) {
    use copypasta::ClipboardProvider;
    copypasta::ClipboardContext::new()
        .unwrap()
        .set_contents(out)
        .unwrap()
}

macro_rules! result {
    ($var: expr) => {
        let mut s = format!("{:?}", $var);
        if s.starts_with('"') && s.ends_with('"') {
            s = s[1..s.len() - 1].to_string();
        }
        println!("result: {}", s);
        crate::utils::copy(s);
    };
    ($format: literal, $($args: expr),+) => {
        let s = format!($format, $($args),+);
        println!("result: {}", s);
        crate::utils::copy(s);
    };
}

pub fn parse_u(input: &str) -> usize {
    <usize as FromStr>::from_str(input).unwrap_or_else(|_| panic!("cannot parse >{}<", input))
}
pub fn parse(input: &str) -> isize {
    <isize as FromStr>::from_str(input).unwrap_or_else(|_| panic!("cannot parse >{}<", input))
}
pub fn parse_c(input: char) -> usize {
    input
        .to_digit(10)
        .unwrap_or_else(|| panic!("cannot parse >{}<", input)) as usize
}

pub fn comma_values<T: FromStr>(input: &str) -> Vec<T> {
    input
        .split(',')
        .filter_map(|s| s.parse::<T>().ok())
        .to_vec()
}

pub trait SliceExt {
    type Item;
    #[allow(clippy::needless_lifetimes)]
    fn two_muts<'a>(
        &'a mut self,
        a: usize,
        b: usize,
    ) -> Option<(&'a mut Self::Item, &'a mut Self::Item)>;
}

impl<T> SliceExt for [T] {
    type Item = T;
    #[allow(clippy::needless_lifetimes)]
    fn two_muts<'a>(
        &'a mut self,
        a: usize,
        b: usize,
    ) -> Option<(&'a mut Self::Item, &'a mut Self::Item)> {
        if a != b && a < self.len() && b < self.len() {
            // SAFETY:
            // - `a` and `b` are different, leading to two different mutable references.
            // - `a` and `b` are both in bounds, leading to valid pointers.
            // - The lifetime bounds on the return value ensures that `self` stays mutably
            //   borrowed for the duration of both references.
            unsafe {
                let p = self.as_mut_ptr();
                Some((&mut *p.add(a), &mut *p.add(b)))
            }
        } else {
            None
        }
    }
}

pub trait DiffExt {
    fn abs_diff(self, other: Self) -> Self;
}
impl<T: cgmath::BaseNum> DiffExt for T {
    fn abs_diff(self, other: T) -> T {
        if self > other {
            self - other
        } else {
            other - self
        }
    }
}

pub fn abs_diff<T: cgmath::BaseNum>(a: T, b: T) -> T {
    a.abs_diff(b)
}

#[derive(Debug, Clone, Copy, FromScanf)]
pub enum Op {
    #[sscanf("+")]
    Add,
    #[sscanf("-")]
    Sub,
    #[sscanf("*")]
    Mul,
    #[sscanf("/")]
    Div,
}
impl Op {
    pub fn apply<N: cgmath::BaseNum>(&self, a: N, b: N) -> N {
        match self {
            Op::Add => a + b,
            Op::Sub => a - b,
            Op::Mul => a * b,
            Op::Div => a / b,
        }
    }
    pub fn inverse_left<N: cgmath::BaseNum>(&self, lhs: N, out: N) -> N {
        // out = lhs op <return>
        match self {
            Op::Add => out - lhs,
            Op::Sub => lhs - out,
            Op::Mul => out / lhs,
            Op::Div => lhs / out,
        }
    }
    pub fn inverse_right<N: cgmath::BaseNum>(&self, rhs: N, out: N) -> N {
        // out = <return> op rhs
        match self {
            Op::Add => out - rhs,
            Op::Sub => out + rhs,
            Op::Mul => out / rhs,
            Op::Div => out * rhs,
        }
    }
}
impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone, Copy, FromScanf)]
pub enum Comp {
    #[sscanf("==")]
    Eq,
    #[sscanf("!=")]
    Ne,
    #[sscanf("<")]
    Lt,
    #[sscanf("<=")]
    Le,
    #[sscanf(">")]
    Gt,
    #[sscanf(">=")]
    Ge,
}

impl Comp {
    pub fn apply<N: cgmath::BaseNum>(&self, a: N, b: N) -> bool {
        match self {
            Comp::Eq => a == b,
            Comp::Ne => a != b,
            Comp::Lt => a < b,
            Comp::Le => a <= b,
            Comp::Gt => a > b,
            Comp::Ge => a >= b,
        }
    }
}
impl std::fmt::Display for Comp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Comp::Eq => write!(f, "=="),
            Comp::Ne => write!(f, "!="),
            Comp::Lt => write!(f, "<"),
            Comp::Le => write!(f, "<="),
            Comp::Gt => write!(f, ">"),
            Comp::Ge => write!(f, ">="),
        }
    }
}

pub fn binary_search(
    mut lower: usize,
    upper: impl Into<Option<usize>>,
    mut is_less_than: impl FnMut(usize) -> bool,
) -> usize {
    let mut upper = if let Some(upper) = upper.into() {
        upper
    } else {
        let mut candidate = lower + 1;
        while !is_less_than(candidate) {
            lower = candidate;
            candidate = (candidate + 1) * 2;
        }
        candidate
    };

    while upper - lower > 1 {
        let mid = lower + (upper - lower) / 2;
        if is_less_than(mid) {
            upper = mid;
        } else {
            lower = mid;
        }
    }
    lower
}

pub fn binary_search_i(
    mut lower: isize,
    upper: impl Into<Option<isize>>,
    mut is_less_than: impl FnMut(isize) -> bool,
) -> isize {
    let mut upper = if let Some(upper) = upper.into() {
        upper
    } else {
        let mut candidate = (lower + 1).max(0);
        while !is_less_than(candidate) {
            lower = candidate;
            candidate = (candidate + 1) * 2;
        }
        candidate
    };

    while upper - lower > 1 {
        let mid = lower + (upper - lower) / 2;
        if is_less_than(mid) {
            upper = mid;
        } else {
            lower = mid;
        }
    }
    lower
}

pub fn detect_loop<T: Hash + std::cmp::Eq>(total_iterations: u128, mut f: impl FnMut() -> T) -> T {
    let mut seen = HashMap::<T, u128>::new();
    let mut v;
    let mut target_number = total_iterations;
    for i in 0..total_iterations {
        v = f();
        match seen.entry(v) {
            Entry::Occupied(e) => {
                let prev = *e.get();
                let loop_len = i - prev;
                let remaining = (total_iterations - i) % loop_len;
                target_number = prev + remaining;
                break;
            }
            Entry::Vacant(e) => {
                e.insert(i);
            }
        }
    }
    seen.into_iter()
        .find(|(_, i)| *i == target_number - 1)
        .unwrap()
        .0
}

pub fn detect_increment_loop<T: Hash + std::cmp::Eq>(
    total_iterations: u128,
    mut f: impl FnMut() -> (T, i128),
    min_runs: u128,
) -> i128 {
    let mut seen = HashMap::<T, (u128, i128)>::new();
    let mut offset = None;
    let mut i = 0;
    let mut x = 0;
    while i < total_iterations {
        let res = f();
        x = res.1;
        i += 1;
        if i < min_runs || offset.is_some() {
            continue;
        }
        match seen.entry(res.0) {
            Entry::Occupied(e) => {
                let (prev, prev_x) = *e.get();
                let loop_len = i - prev;
                let loop_gain = x - prev_x;
                let loops_remaining = (total_iterations - i) / loop_len;
                i += loops_remaining * loop_len;
                offset = Some(loops_remaining as i128 * loop_gain);
            }
            Entry::Vacant(e) => {
                e.insert((i, x));
            }
        }
    }
    x + offset.unwrap_or(0)
}
