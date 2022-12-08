use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Grid<T>(Vec<Vec<T>>);

impl<T> Grid<T> {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn new_default((w, h): Point) -> Self
    where
        T: Default,
    {
        Self(
            std::iter::repeat_with(|| std::iter::repeat_with(Default::default).take(w).to_vec())
                .take(h)
                .to_vec(),
        )
    }
    pub fn new_clone((w, h): Point, src: T) -> Self
    where
        T: Clone,
    {
        Self(vec![vec![src; w]; h])
    }

    pub fn extend_default(&mut self, (w, h): Point)
    where
        T: Default,
    {
        if w > self.w() {
            let diff = w - self.w();
            for row in &mut self.0 {
                row.extend(std::iter::repeat_with(Default::default).take(diff));
            }
        }
        while h > self.h() {
            self.0
                .push(std::iter::repeat_with(Default::default).take(w).to_vec());
        }
    }
    pub fn extend_by_default(&mut self, (dw, dh): Point)
    where
        T: Default,
    {
        self.extend_default((self.w() + dw, self.h() + dh));
    }
    pub fn extend_clone(&mut self, (w, h): Point, src: T)
    where
        T: Clone,
    {
        if w > self.w() {
            for row in &mut self.0 {
                row.resize(w, src.clone());
            }
        }
        while h > self.h() {
            self.0.push(vec![src.clone(); w]);
        }
    }
    pub fn extend_by_clone(&mut self, (dw, dh): Point, src: T)
    where
        T: Clone,
    {
        self.extend_clone((self.w() + dw, self.h() + dh), src);
    }

    pub fn w(&self) -> usize {
        self.0.get(0).map(|v| v.len()).unwrap_or(0)
    }
    pub fn h(&self) -> usize {
        self.len()
    }
    pub fn size(&self) -> Point {
        (self.w(), self.h())
    }
    pub fn bounds(&self) -> Point {
        self.size()
    }

    pub fn in_bounds(&self, (x, y): (isize, isize)) -> bool {
        x >= 0 && y >= 0 && x < self.w() as isize && y < self.h() as isize
    }
    pub fn contains(&self, (x, y): Point) -> bool {
        x < self.w() && y < self.h()
    }
    pub fn map_bounds(&self, (x, y): (isize, isize)) -> Option<Point> {
        if x < 0 || y < 0 || x >= self.w() as isize || y >= self.h() as isize {
            return None;
        }
        Some((x as usize, y as usize))
    }

    pub fn grid_index_iter(&self) -> impl Iterator<Item = Point> {
        let (w, h) = self.bounds();
        (0..h).flat_map(move |x| (0..w).map(move |y| (x, y)))
    }
    pub fn grid_iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter().flat_map(|r| r.iter())
    }
    pub fn grid_iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.0.iter_mut().flat_map(|r| r.iter_mut())
    }
    pub fn grid_iter_index(&self) -> impl Iterator<Item = (Point, &T)> {
        self.0
            .iter()
            .enumerate()
            .flat_map(|(y, r)| r.iter().enumerate().map(move |(x, v)| ((x, y), v)))
    }
    pub fn grid_iter_mut_index(&mut self) -> impl Iterator<Item = (Point, &mut T)> {
        self.0
            .iter_mut()
            .enumerate()
            .flat_map(|(y, r)| r.iter_mut().enumerate().map(move |(x, v)| ((x, y), v)))
    }

    pub fn get(&self, (x, y): Point) -> Option<&T> {
        self.0.get(y).and_then(move |v| v.get(x))
    }
    pub fn get_mut(&mut self, (x, y): Point) -> Option<&mut T> {
        self.0.get_mut(y).and_then(move |v| v.get_mut(x))
    }

    pub fn row(&self, y: usize) -> impl DoubleEndedIterator<Item = &T> {
        self.0[y].iter()
    }
    pub fn row_mut(&mut self, y: usize) -> impl DoubleEndedIterator<Item = &mut T> {
        self.0[y].iter_mut()
    }
    pub fn col(&self, x: usize) -> impl DoubleEndedIterator<Item = &T> {
        self.iter().map(move |r| &r[x])
    }
    pub fn col_mut(&mut self, x: usize) -> impl DoubleEndedIterator<Item = &mut T> {
        self.iter_mut().map(move |r| &mut r[x])
    }

    pub fn checked_move(&self, pos: Point, dir: Dir) -> Option<Point> {
        dir.bounded_add(pos, self.bounds())
    }
    pub fn dir_iter<'a>(&'a self, start: Point, dir: Dir) -> impl Iterator<Item = Point> + 'a {
        std::iter::successors(self.checked_move(start, dir), move |&p| {
            self.checked_move(p, dir)
        })
    }

    pub fn find(&self, t: T) -> Option<Point>
    where
        T: PartialEq,
    {
        self.0.iter().enumerate().find_map(|(y, row)| {
            row.iter()
                .enumerate()
                .find(|(_, v)| **v == t)
                .map(move |(x, _)| (x, y))
        })
    }
    pub fn find_all(&self, t: T) -> impl Iterator<Item = Point> + '_
    where
        T: PartialEq,
    {
        self.0
            .iter()
            .enumerate()
            .flat_map(move |(y, row)| row.iter().enumerate().map(move |(x, v)| (x, y, v)))
            .filter(move |(_, _, v)| **v == t)
            .map(move |(x, y, _)| (x, y))
    }

    pub fn manhattan(&self) -> ManhattanNeighborhood {
        ManhattanNeighborhood::new(self.w(), self.h())
    }
    pub fn moore(&self) -> MooreNeighborhood {
        MooreNeighborhood::new(self.w(), self.h())
    }

    pub fn is_on_border(&self, p: Point, border: Dir) -> bool {
        match border {
            Dir::Left => p.1 == 0,
            Dir::Up => p.0 == 0,
            Dir::Right => p.0 == self.w() - 1,
            Dir::Down => p.1 == self.h() - 1,
        }
    }
    pub fn is_on_any_border(&self, p: Point) -> bool {
        let (w, h) = self.bounds();
        p.0 == 0 || p.1 == 0 || p.0 == w - 1 || p.1 == h - 1
    }
    pub fn border(&self, border: Dir) -> Box<dyn DoubleEndedIterator<Item = &T> + '_> {
        match border {
            Dir::Left => Box::new(self.col(0)),
            Dir::Up => Box::new(self.row(0)),
            Dir::Right => Box::new(self.col(self.w() - 1)),
            Dir::Down => Box::new(self.row(self.h() - 1)),
        }
    }

    pub fn trim_to(&mut self, (w, h): Point) {
        self.0.truncate(h);
        for row in &mut self.0 {
            row.truncate(w);
        }
    }
    pub fn trim_with(&mut self, mut empty: impl FnMut(&T) -> bool) -> (usize, usize, usize, usize) {
        let mut top = 0;
        while top < self.h() && self.row(top).all(&mut empty) {
            top += 1;
        }
        self.splice(..top, std::iter::empty());
        if self.is_empty() {
            return (top, 0, 0, 0);
        }

        let mut bottom = 0;
        while self.row(self.h() - 1).all(&mut empty) {
            self.pop();
            bottom += 1;
        }

        let mut left = 0;
        while self.col(left).all(&mut empty) {
            left += 1;
        }
        self.iter_mut().for_each(|r| {
            r.splice(..left, std::iter::empty());
        });

        let mut right = 0;
        while self.col(self.w() - 1).all(&mut empty) {
            self.iter_mut().for_each(|r| {
                r.pop();
            });
            right += 1;
        }
        (top, bottom, left, right)
    }

    pub fn count_with(&self, mut f: impl FnMut(&T) -> bool) -> usize {
        self.grid_iter().filter(|x| f(*x)).count()
    }
    pub fn sum(&self) -> T
    where
        T: std::iter::Sum<T> + Copy,
    {
        self.grid_iter().copied().sum()
    }

    pub fn square_ring_delta_iterator(
        &self,
        (x, y): Point,
        radius: usize,
    ) -> impl Iterator<Item = (Point, (isize, isize))> + '_ {
        let radius = radius as isize;
        let x = x as isize;
        let y = y as isize;
        (-radius..=radius)
            .map(move |dx| (dx, -radius))
            .chain((-radius + 1..radius).map(move |dy| (-radius, dy)))
            .chain((-radius + 1..radius).map(move |dy| (radius, dy)))
            .chain((-radius..=radius).map(move |dx| (dx, radius)))
            .filter_map(move |(dx, dy)| self.map_bounds((x + dx, y + dy)).map(|p| (p, (dx, dy))))
    }
    pub fn square_ring_iterator(
        &self,
        pos: Point,
        radius: usize,
    ) -> impl Iterator<Item = Point> + '_ {
        self.square_ring_delta_iterator(pos, radius).map(|(p, _)| p)
    }

    pub fn dijkstra(
        &self,
        start: Point,
        goals: &[Point],
        is_walkable: impl Fn(&T) -> bool,
    ) -> HashMap<Point, Path<Point>> {
        let neigh = self.manhattan();
        dijkstra_search(
            |pos, out| {
                for p in neigh.get_all_neighbors(pos) {
                    if is_walkable(self.index(p)) {
                        out.push(p);
                    }
                }
            },
            start,
            goals,
        )
    }
    pub fn a_star(
        &self,
        start: Point,
        goal: Point,
        is_walkable: impl Fn(&T) -> bool,
        heuristic: impl FnMut(Point) -> usize,
    ) -> Option<Path<Point>> {
        let neigh = self.manhattan();
        a_star_search(
            |pos, out| {
                for p in neigh.get_all_neighbors(pos) {
                    if is_walkable(self.index(p)) {
                        out.push(p);
                    }
                }
            },
            start,
            goal,
            heuristic,
        )
    }

    pub fn rotate_counter_clockwise(&mut self) {
        if self.is_empty() {
            return;
        }
        assert_eq!(self.w(), self.h());
        let w = self.w();
        for i in 0..w {
            let (row, rest) = self.0[i..].split_at_mut(1);
            row[0][i + 1..]
                .iter_mut()
                .zip(rest.iter_mut().map(|r| &mut r[i]))
                .for_each(|(a, b)| std::mem::swap(a, b));
        }
        self.reverse();
    }
    pub fn rotate_clockwise(&mut self) {
        if self.is_empty() {
            return;
        }
        assert_eq!(self.w(), self.h());
        let w = self.w();

        // rotate an array: swap x and y, flip one side
        self.reverse();
        for i in 0..w {
            let (row, rest) = self.0[i..].split_at_mut(1);
            row[0][i + 1..]
                .iter_mut()
                .zip(rest.iter_mut().map(|r| &mut r[i]))
                .for_each(|(a, b)| std::mem::swap(a, b));
        }

        // // Rotates a Square in-place
        // //
        // // draws an upside down triangle on the grid:
        // //     0(1 2 3)
        // //     4 5(6)7
        // //     8 9 A B
        // //     C D E F
        // // And swap-rotates each element in the triangle with the corresponding positions
        // // in the rotated versions of the triangle
        // // example:
        // //  1 - 7 - E - 8 - 1
        // // swaps:
        // // (7 - 1)- E - 8
        // //  7 -(E - 1)- 8
        // //  7 - E -(8 - 1)
        // // result:
        // //  7 - E - 8 - 1
        // for i in 0..w / 2 {
        //     for j in i + 1..w - i {
        //         let mut a = (j, i);
        //         for _ in 0..3 {
        //             let b = (a.1, w - a.0 - 1);
        //             self.swap(a, b);
        //             a = b;
        //         }
        //     }
        // }
    }

    pub fn two_muts<'a>(&'a mut self, a: Point, b: Point) -> Option<(&'a mut T, &'a mut T)> {
        if a == b {
            None
        } else if a.1 == b.1 {
            self.0.get_mut(a.1).and_then(|r| r.two_muts(a.0, b.0))
        } else {
            self.0.two_muts(a.1, b.1).and_then(|(ra, rb)| {
                ra.get_mut(a.0)
                    .and_then(|a| rb.get_mut(b.0).map(|b| (a, b)))
            })
        }
    }

    pub fn swap(&mut self, a: Point, b: Point) {
        if a.1 == b.1 {
            self.0[a.1].swap(a.0, b.0);
        } else {
            let (a_slice, b_slice) = self.0.two_muts(a.1, b.1).unwrap();
            std::mem::swap(&mut a_slice[a.0], &mut b_slice[b.0]);
        }
    }

    pub fn fill(&mut self, t: T)
    where
        T: Clone,
    {
        self.0.iter_mut().for_each(|row| row.fill(t.clone()));
    }
    pub fn fill_with(&mut self, mut f: impl FnMut(Point) -> T) {
        self.grid_iter_mut_index().for_each(|(p, t)| *t = f(p));
    }
    pub fn fill_rect(&mut self, tl: Point, br: Point, t: T)
    where
        T: Clone,
    {
        self.0[tl.1..br.1]
            .iter_mut()
            .for_each(|row| row[tl.0..br.0].fill(t.clone()));
    }
    pub fn fill_rect_with(&mut self, tl: Point, br: Point, mut f: impl FnMut(Point) -> T) {
        self.0
            .iter_mut()
            .enumerate()
            .take(br.1)
            .skip(tl.1)
            .for_each(|(y, row)| {
                row.iter_mut()
                    .enumerate()
                    .take(br.0)
                    .skip(tl.0)
                    .for_each(|(x, cell)| *cell = f((x, y)));
            });
    }

    pub fn map<O>(&self, mut f: impl FnMut(&T) -> O) -> Grid<O> {
        let inner = self
            .0
            .iter()
            .map(|row| row.iter().map(&mut f).collect())
            .collect();
        Grid(inner)
    }
}

impl Grid<bool> {
    pub fn print_hashtag(&self) {
        self.print('#', '.')
    }
    pub fn print(&self, filled: char, empty: char) {
        for row in self.iter() {
            print_dotted_line(row, filled, empty);
        }
        println!();
    }
    pub fn trim(&mut self) -> (usize, usize, usize, usize) {
        self.trim_with(|t| !*t)
    }
    pub fn count(&mut self) -> usize {
        self.count_with(|t| *t)
    }
}

impl Grid<char> {
    pub fn print(&self) {
        for row in self.iter() {
            for c in row {
                print!("{}", c);
            }
            println!();
        }
    }
}
impl<T> Grid<T> {
    pub fn print_any<D: std::fmt::Display>(&self, f: impl Fn(&T) -> D) {
        for row in self.iter() {
            for c in row {
                print!("{}", f(c));
            }
            println!();
        }
    }
}

impl<T> From<Vec<Vec<T>>> for Grid<T> {
    fn from(src: Vec<Vec<T>>) -> Self {
        Self(src)
    }
}
impl<'a, T, A> From<&'a [A]> for Grid<T>
where
    A: AsRef<[T]>,
    T: Clone,
{
    fn from(src: &'a [A]) -> Self {
        Self(src.iter().map(|a| a.as_ref().to_vec()).to_vec())
    }
}

impl<T> Index<Point> for Grid<T> {
    type Output = T;
    fn index(&self, p: Point) -> &Self::Output {
        match self.get(p) {
            Some(t) => t,
            None => panic!("size is {:?} but index is {:?}", self.size(), p),
        }
    }
}
impl<T> IndexMut<Point> for Grid<T> {
    fn index_mut(&mut self, p: Point) -> &mut Self::Output {
        let size = self.size();
        match self.get_mut(p) {
            Some(t) => t,
            None => panic!("size is {:?} but index is {:?}", size, p),
        }
    }
}
impl<'a, T> Index<&'a Point> for Grid<T> {
    type Output = T;
    fn index(&self, p: &Point) -> &Self::Output {
        &self[*p]
    }
}
impl<'a, T> IndexMut<&'a Point> for Grid<T> {
    fn index_mut(&mut self, p: &Point) -> &mut Self::Output {
        &mut self[*p]
    }
}

impl<T> Index<(isize, isize)> for Grid<T> {
    type Output = T;
    fn index(&self, p: (isize, isize)) -> &Self::Output {
        match self.map_bounds(p).and_then(|p| self.get(p)) {
            Some(t) => t,
            None => panic!("size is {:?} but index is {:?}", self.size(), p),
        }
    }
}
impl<T> IndexMut<(isize, isize)> for Grid<T> {
    fn index_mut(&mut self, p: (isize, isize)) -> &mut Self::Output {
        let size = self.size();
        match self.map_bounds(p).and_then(move |p| self.get_mut(p)) {
            Some(t) => t,
            None => panic!("size is {:?} but index is {:?}", size, p),
        }
    }
}

impl<T> Index<usize> for Grid<T> {
    type Output = Vec<T>;
    fn index(&self, p: usize) -> &Self::Output {
        &self.0[p]
    }
}
impl<T> IndexMut<usize> for Grid<T> {
    fn index_mut(&mut self, p: usize) -> &mut Self::Output {
        &mut self.0[p]
    }
}

impl<T> Deref for Grid<T> {
    type Target = Vec<Vec<T>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> DerefMut for Grid<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, FromScanf)]
#[sscanf(format = "{:/[.#]+/}")]
struct HashtagLine(#[sscanf(map = |s: &str| dotted_line(s, '#'))] pub Vec<bool>);

impl Deref for HashtagLine {
    type Target = Vec<bool>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn hashtag_line(input: &str) -> Vec<bool> {
    dotted_line(input, '#')
}
pub fn dotted_line(input: &str, non_dot: char) -> Vec<bool> {
    input.chars().map(|c| c == non_dot).to_vec()
}
pub fn hashtag_grid(input: &str) -> Grid<bool> {
    dotted_grid(input, '#')
}
pub fn dotted_grid(input: &str, non_dot: char) -> Grid<bool> {
    input
        .lines()
        .map(|line| dotted_line(line, non_dot))
        .to_vec()
        .into()
}
pub fn char_grid(input: &str) -> Grid<char> {
    input
        .lines()
        .map(|line| line.chars().to_vec())
        .to_vec()
        .into()
}
pub fn digit_grid(input: &str) -> Grid<usize> {
    input
        .lines()
        .map(|line| line.chars().map(parse_c).to_vec())
        .to_vec()
        .into()
}
pub fn number_grid_whitespace<'a>(mut input: impl Iterator<Item = &'a str>) -> Grid<isize> {
    input
        .by_ref()
        .take_while(|l| !l.is_empty())
        .map(|l| l.split_whitespace().map(parse).to_vec())
        .to_vec()
        .into()
}
pub fn number_grid<'a>(mut input: impl Iterator<Item = &'a str>, separator: char) -> Grid<isize> {
    input
        .map(|l| l.split(separator).map(parse).to_vec())
        .to_vec()
        .into()
}

pub fn print_hashtag_line(line: &[bool]) {
    print_dotted_line(line, '#', '.')
}
pub fn print_dotted_line(line: &[bool], filled: char, empty: char) {
    for c in line {
        print!("{}", if *c { filled } else { empty })
    }
    println!();
}

#[test]
fn test_rotate() {
    // 1 2
    // 4 3
    let mut grid = Grid::from(vec![vec![1, 2], vec![4, 3]]);
    let original = grid.clone();
    // 4 1
    // 3 2
    grid.rotate_clockwise();
    assert_eq!(grid.0, [[4, 1], [3, 2]]);
    // 3 4
    // 2 1
    grid.rotate_clockwise();
    assert_eq!(grid.0, [[3, 4], [2, 1]]);
    // 2 3
    // 1 4
    grid.rotate_clockwise();
    assert_eq!(grid.0, [[2, 3], [1, 4]]);
    grid.rotate_clockwise();
    assert_eq!(grid.0, original.0);
}
