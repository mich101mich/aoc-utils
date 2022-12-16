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
pub trait IterMapExt<K: Hash + Eq, V> {
    fn to_map(self) -> HashMap<K, V>;
}
impl<K: Hash + Eq, V, I: Iterator<Item = (K, V)>> IterMapExt<K, V> for I {
    fn to_map(self) -> HashMap<K, V> {
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
            acc = self
                .iter
                .by_ref()
                .take_while(|x| !(self.predicate)(x))
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

pub fn manhattan_ring_iter(center: (isize, isize), radius: isize) -> ManhattanRingIterator {
    ManhattanRingIterator {
        center,
        radius,
        current: Some((center.0, center.1 - radius)),
        side: Dir::Up,
    }
}

pub struct ManhattanRingIterator {
    center: (isize, isize),
    radius: isize,
    current: Option<(isize, isize)>,
    side: Dir,
}

impl Iterator for ManhattanRingIterator {
    type Item = (isize, isize);

    fn next(&mut self) -> Option<Self::Item> {
        let mut current = self.current?;

        let (delta, target) = match self.side {
            Dir::Up => ((1, 1), (self.center.0 + self.radius, self.center.1)),
            Dir::Right => ((-1, 1), (self.center.0, self.center.1 + self.radius)),
            Dir::Down => ((-1, -1), (self.center.0 - self.radius, self.center.1)),
            Dir::Left => ((1, -1), (self.center.0, self.center.1 - self.radius)),
        };

        current.0 += delta.0;
        current.1 += delta.1;
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

pub fn square_ring_delta_iterator(
    center: (isize, isize),
    radius: isize,
) -> SquareRingDeltaIterator {
    SquareRingDeltaIterator {
        center,
        radius,
        current: Some((center.0 - radius, center.1 - radius)),
        side: Dir::Up,
    }
}

pub struct SquareRingDeltaIterator {
    center: (isize, isize),
    radius: isize,
    current: Option<(isize, isize)>,
    side: Dir,
}

impl Iterator for SquareRingDeltaIterator {
    type Item = (isize, isize);

    fn next(&mut self) -> Option<Self::Item> {
        let mut current = self.current?;

        let (delta, target) = match self.side {
            Dir::Up => (
                (1, 0),
                (self.center.0 + self.radius, self.center.1 - self.radius),
            ),
            Dir::Right => (
                (0, 1),
                (self.center.0 + self.radius, self.center.1 + self.radius),
            ),
            Dir::Down => (
                (-1, 0),
                (self.center.0 - self.radius, self.center.1 + self.radius),
            ),
            Dir::Left => (
                (0, -1),
                (self.center.0 - self.radius, self.center.1 - self.radius),
            ),
        };

        current.0 += delta.0;
        current.1 += delta.1;
        if current == target {
            self.side = self.side.clockwise();
            if self.side == Dir::Up {
                self.current = None;
                return None;
            }
        }
        self.current = Some(current);
        Some((current.0 - self.center.0, current.1 - self.center.1))
    }
}

pub fn square_ring_iterator(center: (isize, isize), radius: isize) -> SquareRingIterator {
    SquareRingIterator(square_ring_delta_iterator(center, radius))
}

pub struct SquareRingIterator(SquareRingDeltaIterator);

impl Iterator for SquareRingIterator {
    type Item = (isize, isize);

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|(dx, dy)| (self.0.center.0 + dx, self.0.center.1 + dy))
    }
}
