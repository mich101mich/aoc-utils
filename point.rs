use super::*;
use cgmath::BaseNum;

pub type Point = cgmath::Vector2<usize>;
pub type PointI = cgmath::Vector2<isize>;

pub type Point3D = cgmath::Vector3<usize>;
pub type Point3DI = cgmath::Vector3<isize>;

pub trait PointExt<S: BaseNum> {
    fn abs_diff(self, other: Self) -> Self;
    fn less_than(self, other: Self) -> bool;
    fn manhattan(self, other: Self) -> S;
    fn moore(self, other: Self) -> S;
}
impl<S: BaseNum + Ord> PointExt<S> for cgmath::Vector2<S> {
    fn abs_diff(self, other: Self) -> Self {
        Self::new(self.x.abs_diff(other.x), self.y.abs_diff(other.y))
    }
    fn less_than(self, other: Self) -> bool {
        self.x < other.x && self.y < other.y
    }
    fn manhattan(self, other: Self) -> S {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }
    fn moore(self, other: Self) -> S {
        let diff = self.abs_diff(other);
        diff.x.max(diff.y)
    }
}

impl<S: BaseNum + Ord> PointExt<S> for cgmath::Vector3<S> {
    fn abs_diff(self, other: Self) -> Self {
        Self::new(
            self.x.abs_diff(other.x),
            self.y.abs_diff(other.y),
            self.z.abs_diff(other.z),
        )
    }
    fn less_than(self, other: Self) -> bool {
        self.x < other.x && self.y < other.y && self.z < other.z
    }
    fn manhattan(self, other: Self) -> S {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y) + self.z.abs_diff(other.z)
    }
    fn moore(self, other: Self) -> S {
        let diff = self.abs_diff(other);
        diff.x.max(diff.y).max(diff.z)
    }
}

pub fn manhattan<S: BaseNum, P: PointExt<S>>(p1: P, p2: P) -> S {
    p1.manhattan(p2)
}
pub fn moore<S: BaseNum + Ord, P: PointExt<S>>(p1: P, p2: P) -> S {
    p1.moore(p2)
}
