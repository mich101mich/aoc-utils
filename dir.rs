use super::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, FromScanf)]
pub enum Dir {
    #[sscanf(r"(?i:up|[un^])")]
    Up,
    #[sscanf(r"(?i:right|[re>])")]
    Right,
    #[sscanf(r"(?i:down|[dsv])")]
    Down,
    #[sscanf(r"(?i:left|[lw<])")]
    Left,
}
pub use Dir::*;

impl Dir {
    pub fn clockwise(self) -> Dir {
        ((self as u8 + 1) % 4).into()
    }
    pub fn counter_clockwise(self) -> Dir {
        ((self as u8 + 3) % 4).into()
    }
    pub fn opposite(self) -> Dir {
        ((self as u8 + 2) % 4).into()
    }
    pub fn num(self) -> usize {
        self.into()
    }
    pub fn all() -> std::iter::Copied<std::slice::Iter<'static, Dir>> {
        [Up, Right, Down, Left].iter().copied()
    }
    pub fn as_delta(self) -> (isize, isize) {
        [(0, -1), (1, 0), (0, 1), (-1, 0)][self as usize]
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
    pub fn bounded_add(self, pos: (usize, usize), bounds: (usize, usize)) -> Option<(usize, usize)> {
        self.checked_add(pos)
            .filter(|p| p.0 < bounds.0 && p.1 < bounds.1)
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
    pub fn to_char(self) -> char {
        ['U', 'R', 'D', 'L'][self as usize]
    }
    pub fn to_char_arrow(self) -> char {
        ['^', '>', 'v', '<'][self as usize]
    }
    pub fn to_char_cardinal(self) -> char {
        ['N', 'E', 'S', 'W'][self as usize]
    }

    pub fn is_vertical(&self) -> bool {
        matches!(self, Up | Down)
    }
    pub fn is_horizontal(&self) -> bool {
        !self.is_vertical()
    }
}

impl std::fmt::Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_char())
    }
}

macro_rules! impl_dir_ops {
    ($($type:ty),+) => {$(
        impl From<$type> for Dir {
            fn from(val: $type) -> Dir {
                [Up, Right, Down, Left][val as usize]
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
