use std::convert::TryInto;
use std::ops::*;
use std::str::FromStr;

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
    pub fn from_difference(from: (isize, isize), to: (isize, isize)) -> Dir {
        let axis_0 = to.0 - from.0;
        let axis_1 = to.1 - from.1;
        if axis_0.abs() > axis_1.abs() {
            if axis_0 > 0 {
                Dir::Right
            } else {
                Dir::Left
            }
        } else if axis_1 > 0 {
            Dir::Down
        } else {
            Dir::Up
        }
    }
}

impl FromStr for Dir {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "Up" | "up" => Dir::Up,
            "Right" | "right" => Dir::Right,
            "Down" | "down" => Dir::Down,
            "Left" | "left" => Dir::Left,
            c if c.len() == 1 => c.chars().next().unwrap().into(),
            s => return Err(format!("Not a Dir: {:?}", s)),
        })
    }
}

impl From<String> for Dir {
    fn from(s: String) -> Self {
        Self::from_str(&s).unwrap()
    }
}
impl From<&'_ str> for Dir {
    fn from(s: &'_ str) -> Self {
        Self::from_str(s).unwrap()
    }
}

impl From<char> for Dir {
    fn from(c: char) -> Self {
        match c {
            'N' | 'n' | 'U' | 'u' | '^' => Dir::Up,
            'E' | 'e' | 'R' | 'r' | '>' => Dir::Right,
            'S' | 's' | 'D' | 'd' | 'v' => Dir::Down,
            'W' | 'w' | 'L' | 'l' | '<' => Dir::Left,
            c => panic!("Not a Dir: '{}'", c),
        }
    }
}

impl sscanf::RegexRepresentation for Dir {
    const REGEX: &'static str = "[Uu]p|[Dd]own|[Ll]eft|[Rr]ight|[NnUuEeRrSsDdEeLl^>v<]";
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
        impl From<Dir> for $type {
            fn from(dir: Dir) -> $type {
                dir as $type
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
        impl Sub<Dir> for ($type, $type) {
            type Output = Self;
            fn sub(self, other: Dir) -> Self {
                let delta = other.as_delta();
                (
                    (self.0 as isize - delta.0) as $type,
                    (self.1 as isize - delta.1) as $type,
                )
            }
        }
        impl SubAssign<Dir> for ($type, $type) {
            fn sub_assign(&mut self, other: Dir) {
                *self = *self - other;
            }
        }
    )+}
}

impl_dir_ops!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);
