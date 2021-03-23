#![allow(unused)]
#![allow(clippy::wrong_self_convention)]

pub use rand::prelude::*;
pub use rayon::prelude::*;
pub use regex::Regex;
pub use sscanf::*;
pub use std::collections::{HashMap, HashSet, VecDeque};
pub use std::io::Write;
pub use std::str::FromStr;

mod neighbors;
pub use neighbors::*;
mod path;
pub use path::*;

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

pub fn hashtag_line(input: &str) -> Vec<bool> {
    dotted_line(input, '#')
}
pub fn dotted_line(input: &str, non_dot: char) -> Vec<bool> {
    input.chars().map(|c| c == non_dot).to_vec()
}
pub fn hashtag_grid(input: &str) -> Vec<Vec<bool>> {
    dotted_grid(input, '#')
}
pub fn dotted_grid(input: &str, non_dot: char) -> Vec<Vec<bool>> {
    input
        .lines()
        .map(|line| dotted_line(line, non_dot))
        .to_vec()
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

/// Rotates a Square in-place
///
/// draws an upside down triangle on the grid:
///     0(1 2 3)
///     4 5(6)7
///     8 9 A B
///     C D E F
/// And swap-rotates each element in the triangle with the corresponding positions
/// in the rotated versions of the triangle
/// example:
///  1 - 7 - E - 8 - 1
/// swaps:
/// (7 - 1)- E - 8
///  7 -(E - 1)- 8
///  7 - E -(8 - 1)
/// result:
///  7 - E - 8 - 1
pub fn rotate_grid_clock<T>(grid: &mut [Vec<T>]) {
    if grid.is_empty() {
        return;
    }
    assert_eq!(grid.len(), grid[0].len());
    let w = grid.len();
    use std::mem::{swap, transmute};
    for i in 0..w / 2 {
        for j in i + 1..w - i {
            let mut a = (i, j);
            for _ in 0..3 {
                let b = (w - a.1 - 1, a.0);
                // SAFETY: trust me, i tested this once in the Rust Playground
                unsafe { swap(transmute(&mut grid[a.0][a.1]), &mut grid[b.0][b.1]) };
                a = b;
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Dir {
    Up,
    Right,
    Down,
    Left,
}
pub use Dir::*;

impl Dir {
    pub fn clockwise(self) -> Dir {
        ((self.num() + 1) % 4).into()
    }
    pub fn counter_clockwise(self) -> Dir {
        ((self.num() + 3) % 4).into()
    }
    pub fn opposite(self) -> Dir {
        ((self.num() + 2) % 4).into()
    }
    pub fn num(self) -> usize {
        self.into()
    }
    pub fn all() -> std::iter::Copied<std::slice::Iter<'static, Dir>> {
        [Up, Right, Down, Left].iter().copied()
    }
    pub fn as_delta(self) -> (isize, isize) {
        [(0, -1), (1, 0), (0, 1), (-1, 0)][self.num()]
    }
    pub fn checked_add(self, pos: (usize, usize)) -> Option<(usize, usize)> {
        let delta = self.as_delta();
        let ret = ((pos.0 as isize + delta.0), (pos.1 as isize + delta.1));
        if ret.0 < 0 || ret.1 < 0 {
            None
        } else {
            Some((ret.0 as usize, ret.1 as usize))
        }
    }
}

macro_rules! impl_dir_ops {
    ($($type:ty),+) => {$(
        impl From<$type> for Dir {
            fn from(val: $type) -> Dir {
                match val {
                    0 => Up,
                    1 => Right,
                    2 => Down,
                    3 => Left,
                    n => panic!("Invalid Dir value: {}", n),
                }
            }
        }
        impl Into<$type> for Dir {
            fn into(self) -> $type {
                self as $type
            }
        }
        impl Add<Dir> for ($type, $type) {
            type Output = Self;
            fn add(self, other: Dir) -> Self {
                let delta = other.as_delta();
                (
                    (self.0 as isize + delta.0) as $type,
                    (self.1 as isize + delta.1) as $type,
                )
            }
        }
        impl AddAssign<Dir> for ($type, $type) {
            fn add_assign(&mut self, other: Dir) {
                *self = *self + other;
            }
        }
    )+}
}

impl_dir_ops!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);
