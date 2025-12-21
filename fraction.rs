use std::{fmt::Display, ops::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fraction {
    /// The numerator of the fraction
    pub num: isize,
    /// The denominator of the fraction. Always greater than 0
    pub denom: isize,
}

impl Fraction {
    pub fn new(mut num: isize, mut denom: isize) -> Self {
        if num == 0 {
            return Self::default();
        }
        let gcd = num::integer::gcd(num, denom);
        num /= gcd;
        denom /= gcd;
        if denom < 0 {
            num = -num;
            denom = -denom;
        }
        assert_ne!(denom, 0);
        Self { num, denom }
    }

    /// Flips the fraction by swapping numerator and denominator.
    ///
    /// Panics if self == 0
    pub fn invert(&self) -> Self {
        Self::new(self.denom, self.num)
    }

    /// Brings two fractions to the same denominator, returning the two numerators
    pub fn to_same_denom(&self, rhs: &Self) -> (isize, isize) {
        (self.num * rhs.denom, rhs.num * self.denom)
    }

    pub fn to_isize(&self) -> isize {
        assert_eq!(self.denom, 1);
        self.num
    }
}

impl Display for Fraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.num, self.denom)
    }
}

impl Default for Fraction {
    fn default() -> Self {
        Self { num: 0, denom: 1 }
    }
}

impl From<isize> for Fraction {
    fn from(num: isize) -> Self {
        Self { num, denom: 1 }
    }
}

impl PartialOrd for Fraction {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Fraction {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let (a, b) = self.to_same_denom(other);
        a.cmp(&b)
    }
}

impl PartialOrd<isize> for Fraction {
    fn partial_cmp(&self, other: &isize) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&Self::from(*other))
    }
}

impl PartialEq<isize> for Fraction {
    fn eq(&self, other: &isize) -> bool {
        self.denom == 1 && self.num == *other
    }
}
impl PartialEq<Fraction> for isize {
    fn eq(&self, other: &Fraction) -> bool {
        other == self
    }
}

impl Add for Fraction {
    type Output = Fraction;
    fn add(self, rhs: Self) -> Self::Output {
        let (a, b) = self.to_same_denom(&rhs);
        Self::new(a + b, self.denom * rhs.denom)
    }
}
impl Sub for Fraction {
    type Output = Fraction;
    fn sub(self, rhs: Self) -> Self::Output {
        let (a, b) = self.to_same_denom(&rhs);
        Self::new(a - b, self.denom * rhs.denom)
    }
}

impl Mul for Fraction {
    type Output = Fraction;
    fn mul(self, rhs: Self) -> Self::Output {
        Self::new(self.num * rhs.num, self.denom * rhs.denom)
    }
}
impl Div for Fraction {
    type Output = Fraction;
    #[allow(clippy::suspicious_arithmetic_impl)]
    fn div(self, rhs: Self) -> Self::Output {
        self * rhs.invert()
    }
}

impl AddAssign for Fraction {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}
impl SubAssign for Fraction {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}
impl MulAssign for Fraction {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}
impl DivAssign for Fraction {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl Mul<isize> for Fraction {
    type Output = Fraction;
    fn mul(self, rhs: isize) -> Self::Output {
        self * Self::from(rhs)
    }
}
impl Div<isize> for Fraction {
    type Output = Fraction;
    fn div(mut self, rhs: isize) -> Self::Output {
        self / Self::from(rhs)
    }
}

impl MulAssign<isize> for Fraction {
    fn mul_assign(&mut self, rhs: isize) {
        *self = *self * rhs
    }
}
impl DivAssign<isize> for Fraction {
    fn div_assign(&mut self, rhs: isize) {
        *self = *self / rhs
    }
}
