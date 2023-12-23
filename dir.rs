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
    pub fn from_char(c: char) -> Option<Self> {
        match c.to_ascii_lowercase() {
            'u' | '^' | 'n' => Some(Up),
            'r' | '>' | 'e' => Some(Right),
            'd' | 'v' | 's' => Some(Down),
            'l' | '<' | 'w' => Some(Left),
            _ => None,
        }
    }
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
    pub fn as_delta(self) -> PointI {
        [p2(0, -1), p2(1, 0), p2(0, 1), p2(-1, 0)][self as usize]
    }
    pub fn checked_add(self, pos: Point) -> Option<Point> {
        let delta = self.as_delta();
        Some(p2(
            pos.x.checked_add_signed(delta.x)?,
            pos.y.checked_add_signed(delta.y)?,
        ))
    }
    pub fn bounded_add(self, pos: Point, bounds: Point) -> Option<Point> {
        self.checked_add(pos).filter(|p| p.less_than(bounds))
    }
    pub fn wrapping_add(self, pos: Point, bounds: Point) -> Point {
        let delta = self.as_delta();
        p2(
            ((pos.x + bounds.x).saturating_add_signed(delta.x)) % bounds.x,
            ((pos.y + bounds.y).saturating_add_signed(delta.y)) % bounds.y,
        )
    }
    pub fn from_difference(from: PointI, to: PointI) -> Dir {
        let diff = to - from;
        if diff.x.abs() > diff.y.abs() {
            if diff.x > 0 {
                Dir::Right
            } else {
                Dir::Left
            }
        } else if diff.y > 0 {
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
        impl Add<Dir> for cgmath::Vector2<$type> {
            type Output = Self;
            fn add(self, other: Dir) -> Self {
                let delta = other.as_delta();
                Self::new(
                    (self.x as isize + delta.x) as $type,
                    (self.y as isize + delta.y) as $type,
                )
            }
        }
        impl AddAssign<Dir> for cgmath::Vector2<$type> {
            fn add_assign(&mut self, other: Dir) {
                *self = *self + other;
            }
        }
        #[allow(clippy::suspicious_arithmetic_impl)]
        impl Sub<Dir> for cgmath::Vector2<$type> {
            type Output = Self;
            fn sub(self, other: Dir) -> Self {
                self + other.opposite()
            }
        }
        #[allow(clippy::suspicious_op_assign_impl)]
        impl SubAssign<Dir> for cgmath::Vector2<$type> {
            fn sub_assign(&mut self, other: Dir) {
                *self += other.opposite();
            }
        }
    )+}
}

impl_dir_ops!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Dir3D {
    PX,
    PY,
    PZ,
    NX,
    NY,
    NZ,
}
use Dir3D::*;

impl Dir3D {
    pub fn clockwise(self) -> Dir3D {
        ((self as u8 + 1) % 6).into()
    }
    pub fn counter_clockwise(self) -> Dir3D {
        ((self as u8 + 5) % 6).into()
    }
    pub fn opposite(self) -> Dir3D {
        ((self as u8 + 3) % 6).into()
    }
    pub fn num(self) -> usize {
        self.into()
    }
    pub fn all() -> std::iter::Copied<std::slice::Iter<'static, Dir3D>> {
        [PX, PY, PZ, NX, NY, NZ].iter().copied()
    }
    pub fn as_delta(self) -> Point3DI {
        [
            p3(1, 0, 0),
            p3(0, 1, 0),
            p3(0, 0, 1),
            p3(-1, 0, 0),
            p3(0, -1, 0),
            p3(0, 0, -1),
        ][self as usize]
    }
    pub fn checked_add(self, pos: Point3D) -> Option<Point3D> {
        let delta = self.as_delta();
        Some(p3(
            pos.x.checked_add_signed(delta.x)?,
            pos.y.checked_add_signed(delta.y)?,
            pos.z.checked_add_signed(delta.z)?,
        ))
    }
    pub fn bounded_add(self, pos: Point3D, bounds: Point3D) -> Option<Point3D> {
        self.checked_add(pos)
            .filter(|p| p.x < bounds.x && p.y < bounds.y && p.z < bounds.z)
    }
    pub fn from_difference(from: Point3DI, to: Point3DI) -> Dir3D {
        let diff = to - from;
        let abs = to.abs_diff(from);
        if abs.x > abs.y && abs.x > abs.z {
            if diff.x > 0 {
                Dir3D::PX
            } else {
                Dir3D::NX
            }
        } else if abs.y > abs.z {
            if diff.y > 0 {
                Dir3D::PY
            } else {
                Dir3D::NY
            }
        } else if diff.z > 0 {
            Dir3D::PZ
        } else {
            Dir3D::NZ
        }
    }
}
impl std::fmt::Display for Dir3D {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PX => write!(f, "+X"),
            PY => write!(f, "+Y"),
            PZ => write!(f, "+Z"),
            NX => write!(f, "-X"),
            NY => write!(f, "-Y"),
            NZ => write!(f, "-Z"),
        }
    }
}

macro_rules! impl_dir_ops_3d {
    ($($type:ty),+) => {$(
        impl From<$type> for Dir3D {
            fn from(val: $type) -> Dir3D {
                [PX, PY, PZ, NX, NY, NZ][val as usize]
            }
        }
        impl From<Dir3D> for $type {
            fn from(dir: Dir3D) -> $type {
                dir as $type
            }
        }
        impl Add<Dir3D> for cgmath::Vector3<$type> {
            type Output = Self;
            fn add(self, other: Dir3D) -> Self {
                let delta = other.as_delta();
                Self::new(
                    (self.x as isize + delta.x) as $type,
                    (self.y as isize + delta.y) as $type,
                    (self.z as isize + delta.z) as $type,
                )
            }
        }
        impl AddAssign<Dir3D> for cgmath::Vector3<$type> {
            fn add_assign(&mut self, other: Dir3D) {
                *self = *self + other;
            }
        }
        #[allow(clippy::suspicious_arithmetic_impl)]
        impl Sub<Dir3D> for cgmath::Vector3<$type> {
            type Output = Self;
            fn sub(self, other: Dir3D) -> Self {
                self + other.opposite()
            }
        }
        #[allow(clippy::suspicious_op_assign_impl)]
        impl SubAssign<Dir3D> for cgmath::Vector3<$type> {
            fn sub_assign(&mut self, other: Dir3D) {
                *self += other.opposite();
            }
        }
    )+}
}

impl_dir_ops_3d!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);
