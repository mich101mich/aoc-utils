#![allow(unused)]
#![allow(clippy::wrong_self_convention)]

pub use rand::prelude::*;
pub use rayon::prelude::*;
pub use regex::Regex;
pub use sscanf::*;
pub use std::cmp::Ordering;
pub use std::collections::{hash_map::Entry, HashMap, HashSet, VecDeque};
pub use std::io::Write;
pub use std::str::FromStr;

mod neighbors;
pub use neighbors::*;
mod path;
pub use path::*;
mod dir;
pub use dir::*;
mod grid;
pub use grid::*;
mod int_code;
pub use int_code::*;

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
        );*
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

pub fn parse_u(input: &str) -> usize {
    usize::from_str(input).unwrap_or_else(|_| panic!("cannot parse >{}<", input))
}
pub fn parse(input: &str) -> isize {
    isize::from_str(input).unwrap_or_else(|_| panic!("cannot parse >{}<", input))
}
pub fn parse_c(input: char) -> usize {
    if ('0'..='9').contains(&input) {
        (input as u8 - b'0') as usize
    } else {
        panic!("{} is not a number", input)
    }
}

pub fn comma_values<T: FromStr>(input: &str) -> Vec<T> {
    input
        .split(',')
        .filter_map(|s| s.parse::<T>().ok())
        .to_vec()
}

pub trait IterExt<T> {
    fn to_vec(self) -> Vec<T>;
    fn to_queue(self) -> VecDeque<T>;
}
impl<T, I: Iterator<Item = T>> IterExt<T> for I {
    fn to_vec(self) -> Vec<T> {
        self.collect()
    }
    fn to_queue(self) -> VecDeque<T> {
        self.collect()
    }
}
pub trait IterHashExt<T: Hash + Eq> {
    fn to_set(self) -> HashSet<T>;
}
impl<T: Hash + Eq, I: Iterator<Item = T>> IterHashExt<T> for I {
    fn to_set(self) -> HashSet<T> {
        self.collect()
    }
}
pub trait IterMapExt<K: Hash + Eq, V> {
    fn to_map(self) -> HashMap<K, V>;
}
impl<K: Hash + Eq, V, I: Iterator<Item = (K, V)>> IterMapExt<K, V> for I {
    fn to_map(self) -> HashMap<K, V> {
        self.collect()
    }
}

pub trait DiffExt {
    fn diff(self, other: usize) -> usize;
}
impl DiffExt for usize {
    fn diff(self, other: usize) -> usize {
        if self > other {
            self - other
        } else {
            other - self
        }
    }
}

pub fn diff(a: usize, b: usize) -> usize {
    a.diff(b)
}
pub fn diff_i(a: isize, b: isize) -> isize {
    (a - b).abs()
}

pub fn manhattan(p1: (usize, usize), p2: (usize, usize)) -> usize {
    p1.0.diff(p2.0) + p1.1.diff(p2.1)
}
pub fn manhattan_i(p1: (isize, isize), p2: (isize, isize)) -> isize {
    diff_i(p1.0, p2.0) + diff_i(p1.1, p2.1)
}

pub fn manhattan_3d(p1: (usize, usize, usize), p2: (usize, usize, usize)) -> usize {
    p1.0.diff(p2.0) + p1.1.diff(p2.1) + p1.2.diff(p2.2)
}
pub fn manhattan_3d_i(p1: (isize, isize, isize), p2: (isize, isize, isize)) -> isize {
    diff_i(p1.0, p2.0) + diff_i(p1.1, p2.1) + diff_i(p1.2, p2.2)
}

pub fn moore(p1: (usize, usize), p2: (usize, usize)) -> usize {
    p1.0.diff(p2.0).max(p1.1.diff(p2.1))
}
pub fn moore_i(p1: (isize, isize), p2: (isize, isize)) -> isize {
    diff_i(p1.0, p2.0).max(diff_i(p1.1, p2.1))
}

pub fn binary_search(start: usize, mut check: impl FnMut(usize) -> bool) -> usize {
    let mut min = start;
    let mut max = start;
    loop {
        if check(max) {
            break;
        }
        min = max;
        max = (max + 1) * 2;
    }
    while max - min > 1 {
        let mid = (max + min) / 2;
        if check(mid) {
            max = mid;
        } else {
            min = mid;
        }
    }
    max
}
pub fn binary_search_i(start: isize, mut check: impl FnMut(isize) -> bool) -> isize {
    let mut min = start;
    let mut max = start;
    loop {
        if check(max) {
            break;
        }
        min = max;
        max = (max + 1) * 2;
    }
    while max - min > 1 {
        let mid = (max + min) / 2;
        if check(mid) {
            max = mid;
        } else {
            min = mid;
        }
    }
    max
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
