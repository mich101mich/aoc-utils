use super::*;

pub trait IterExt<T> {
    fn to_vec(self) -> Vec<T>;

    fn to_queue(self) -> VecDeque<T>;

    fn take_while_inclusive<P>(self, predicate: P) -> TakeWhileInclusive<Self, P>
    where
        Self: Iterator<Item = T> + Sized,
        P: FnMut(&T) -> bool,
    {
        TakeWhileInclusive {
            iter: self,
            predicate,
            flag: false,
        }
    }

    fn split_fold<B, P, F, G>(self, predicate: P, init: G, fold_op: F) -> Split<T, Self, B, P, F, G>
    where
        P: FnMut(&T) -> bool,
        G: FnMut() -> B,
        F: FnMut(B, T) -> B,
        Self: Iterator<Item = T> + Sized,
    {
        Split {
            iter: self,
            predicate,
            init,
            fold_op,
        }
    }

    #[allow(clippy::type_complexity)]
    fn split_to_vec<P>(
        self,
        predicate: P,
    ) -> Split<T, Self, Vec<T>, P, fn(Vec<T>, T) -> Vec<T>, fn() -> Vec<T>>
    where
        P: FnMut(&T) -> bool,
        Self: Iterator<Item = T> + Sized,
    {
        self.split_fold(predicate, Vec::new, |mut v, t| {
            v.push(t);
            v
        })
    }
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

pub trait IterOrdExt<T: PartialOrd + Eq> {
    fn to_ord_set(self) -> BTreeSet<T>;
}
impl<T: Ord + Eq, I: Iterator<Item = T>> IterOrdExt<T> for I {
    fn to_ord_set(self) -> BTreeSet<T> {
        self.collect()
    }
}

pub trait IterHashMapExt<K: Hash + Eq, V> {
    fn to_map(self) -> HashMap<K, V>;
}
impl<K: Hash + Eq, V, I: Iterator<Item = (K, V)>> IterHashMapExt<K, V> for I {
    fn to_map(self) -> HashMap<K, V> {
        self.collect()
    }
}

pub trait IterOrdMapExt<K: Ord + Eq, V> {
    fn to_ord_map(self) -> BTreeMap<K, V>;
}
impl<K: Ord + Eq, V, I: Iterator<Item = (K, V)>> IterOrdMapExt<K, V> for I {
    fn to_ord_map(self) -> BTreeMap<K, V> {
        self.collect()
    }
}

pub trait IterStringExt {
    fn to_string(self) -> String;
}
impl<I: Iterator<Item = char>> IterStringExt for I {
    fn to_string(self) -> String {
        self.collect()
    }
}

pub struct Split<T, I, B, P, F, G>
where
    I: Iterator<Item = T>,
    P: FnMut(&T) -> bool,
    G: FnMut() -> B,
    F: FnMut(B, T) -> B,
{
    iter: I,
    predicate: P,
    init: G,
    fold_op: F,
}
impl<T, I, B, P, F, G> Iterator for Split<T, I, B, P, F, G>
where
    I: Iterator<Item = T>,
    P: FnMut(&T) -> bool,
    G: FnMut() -> B,
    F: FnMut(B, T) -> B,
{
    type Item = B;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(x) = self.iter.next() {
            let mut acc = (self.init)();
            if (self.predicate)(&x) {
                return Some(acc);
            }
            acc = (self.fold_op)(acc, x);
            let predicate = &mut self.predicate;
            acc = self
                .iter
                .by_ref()
                .take_while(|x| !predicate(x))
                .fold(acc, &mut self.fold_op);
            Some(acc)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

pub struct TakeWhileInclusive<I, P>
where
    I: Iterator,
    P: FnMut(&I::Item) -> bool,
{
    iter: I,
    flag: bool,
    predicate: P,
}

impl<I, P> Iterator for TakeWhileInclusive<I, P>
where
    I: Iterator,
    P: FnMut(&I::Item) -> bool,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        if self.flag {
            None
        } else {
            let x = self.iter.next()?;
            self.flag = !(self.predicate)(&x);
            Some(x)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.flag {
            (0, Some(0))
        } else {
            let (_, upper) = self.iter.size_hint();
            (0, upper) // can't know a lower bound, due to the predicate
        }
    }
}

pub fn manhattan_ring_iter(center: PointI, radius: isize) -> ManhattanRingIterator {
    ManhattanRingIterator {
        center,
        radius,
        next_value: Some(p2(center.x, center.y - radius)),
        side: Dir::Up,
    }
}

pub struct ManhattanRingIterator {
    center: PointI,
    radius: isize,
    next_value: Option<PointI>,
    side: Dir,
}

impl Iterator for ManhattanRingIterator {
    type Item = PointI;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next_value = self.next_value?;
        let ret = next_value;

        let (delta, target) = match self.side {
            Dir::Up => (p2(1, 1), p2(self.center.x + self.radius, self.center.y)),
            Dir::Right => (p2(-1, 1), p2(self.center.x, self.center.y + self.radius)),
            Dir::Down => (p2(-1, -1), p2(self.center.x - self.radius, self.center.y)),
            Dir::Left => (p2(1, -1), p2(self.center.x, self.center.y - self.radius)),
        };

        next_value += delta;
        self.next_value = Some(next_value);
        if next_value == target {
            self.side = self.side.clockwise();
            if self.side == Dir::Up {
                self.next_value = None;
            }
        }
        Some(ret)
    }
}

pub fn square_ring_iterator(center: PointI, radius: isize) -> SquareRingIterator {
    SquareRingIterator {
        center,
        radius,
        current: Some(p2(center.x - radius, center.y - radius)),
        side: Dir::Up,
    }
}

pub struct SquareRingIterator {
    center: PointI,
    radius: isize,
    current: Option<PointI>,
    side: Dir,
}

impl Iterator for SquareRingIterator {
    type Item = PointI;

    fn next(&mut self) -> Option<Self::Item> {
        let mut current = self.current?;

        let (delta, target) = match self.side {
            Dir::Up => (
                p2(1, 0),
                p2(self.center.x + self.radius, self.center.y - self.radius),
            ),
            Dir::Right => (
                p2(0, 1),
                p2(self.center.x + self.radius, self.center.y + self.radius),
            ),
            Dir::Down => (
                p2(-1, 0),
                p2(self.center.x - self.radius, self.center.y + self.radius),
            ),
            Dir::Left => (
                p2(0, -1),
                p2(self.center.x - self.radius, self.center.y - self.radius),
            ),
        };

        current += delta;
        if current == target {
            self.side = self.side.clockwise();
            if self.side == Dir::Up {
                self.current = None;
                return None;
            }
        }
        self.current = Some(current);
        Some(current)
    }
}

pub fn square_ring_delta_iterator(center: PointI, radius: isize) -> SquareRingDeltaIterator {
    SquareRingDeltaIterator(square_ring_iterator(center, radius))
}

pub struct SquareRingDeltaIterator(SquareRingIterator);

impl Iterator for SquareRingDeltaIterator {
    type Item = PointI;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|pos| pos - self.0.center)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manhattan_ring_iter_test() {
        let center = p2(10, 10);
        let radius = 2;
        let points = manhattan_ring_iter(center, radius).to_vec();
        let expected = vec![
            p2(10, 8),
            p2(11, 9),
            p2(12, 10),
            p2(11, 11),
            p2(10, 12),
            p2(9, 11),
            p2(8, 10),
            p2(9, 9),
        ];
        assert_eq!(points, expected);
    }
}
