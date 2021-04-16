use super::*;

#[derive(Clone, Debug)]
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

    pub fn w(&self) -> usize {
        self.0.get(0).map(|v| v.len()).unwrap_or(0)
    }
    pub fn h(&self) -> usize {
        self.len()
    }
    pub fn bounds(&self) -> Point {
        (self.w(), self.h())
    }

    pub fn in_bounds(&self, (x, y): (isize, isize)) -> bool {
        x >= 0 && y >= 0 && x < self.w() as isize && y < self.h() as isize
    }
    pub fn map_bounds(&self, (x, y): (isize, isize)) -> Option<Point> {
        if x < 0 || y < 0 || x >= self.w() as isize || y >= self.h() as isize {
            return None;
        }
        Some((x as usize, y as usize))
    }

    pub fn grid_index_iter(&self) -> impl Iterator<Item = Point> {
        let (w, h) = self.bounds();
        (0..w).flat_map(move |x| (0..h).map(move |y| (x, y)))
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

    pub fn row(&self, y: usize) -> impl Iterator<Item = &T> {
        self.0[y].iter()
    }
    pub fn row_mut(&mut self, y: usize) -> impl Iterator<Item = &mut T> {
        self.0[y].iter_mut()
    }
    pub fn col(&self, x: usize) -> impl Iterator<Item = &T> {
        self.iter().map(move |r| &r[x])
    }
    pub fn col_mut(&mut self, x: usize) -> impl Iterator<Item = &mut T> {
        self.iter_mut().map(move |r| &mut r[x])
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
    pub fn border(&self, border: Dir) -> Box<dyn Iterator<Item = &T> + '_> {
        match border {
            Dir::Left => Box::new(self.col(0)),
            Dir::Up => Box::new(self.row(0)),
            Dir::Right => Box::new(self.col(self.w() - 1)),
            Dir::Down => Box::new(self.row(self.h() - 1)),
        }
    }

    pub fn trim_with(&mut self, mut empty: impl FnMut(&T) -> bool) -> (usize, usize, usize, usize) {
        let mut top = 0;
        while top < self.h() && self.row(top).all(|x| empty(x)) {
            top += 1;
        }
        self.splice(..top, std::iter::empty());
        if self.is_empty() {
            return (top, 0, 0, 0);
        }

        let mut bottom = 0;
        while self.row(self.h() - 1).all(|x| empty(x)) {
            self.pop();
            bottom += 1;
        }

        let mut left = 0;
        while self.col(left).all(|x| empty(x)) {
            left += 1;
        }
        self.iter_mut().for_each(|r| {
            r.splice(..left, std::iter::empty());
        });

        let mut right = 0;
        while self.col(self.w() - 1).all(|x| empty(x)) {
            self.iter_mut().for_each(|r| {
                r.pop();
            });
            right += 1;
        }
        (top, bottom, left, right)
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
            |p| {
                neigh
                    .get_all_neighbors(p)
                    .filter(|p| is_walkable(self.index(*p)))
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
            |p| {
                neigh
                    .get_all_neighbors(p)
                    .filter(|p| is_walkable(self.index(*p)))
            },
            start,
            goal,
            heuristic,
        )
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
    pub fn rotate_clockwise(&mut self) {
        if self.is_empty() {
            return;
        }
        assert_eq!(self.w(), self.h());
        let w = self.w();
        for i in 0..w / 2 {
            for j in i + 1..w - i {
                let mut a = (j, i);
                for _ in 0..3 {
                    let b = (a.1, w - a.0 - 1);
                    self.swap(a, b);
                    a = b;
                }
            }
        }
    }

    pub fn swap(&mut self, mut a: (usize, usize), mut b: (usize, usize)) {
        if a.1 == b.1 {
            self.0[a.1].swap(a.0, b.0);
        } else {
            if a.1 > b.1 {
                std::mem::swap(&mut a, &mut b);
            }
            let (front, back) = self.0.split_at_mut(b.1);
            std::mem::swap(
                front.get_mut(a.1).unwrap().get_mut(a.0).unwrap(),
                back[0].get_mut(b.0).unwrap(),
            );
        }
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
}

impl<T> From<Vec<Vec<T>>> for Grid<T> {
    fn from(src: Vec<Vec<T>>) -> Self {
        Self(src)
    }
}

impl<T> std::ops::Index<Point> for Grid<T> {
    type Output = T;
    fn index(&self, p: Point) -> &Self::Output {
        self.get(p).unwrap()
    }
}
impl<T> std::ops::IndexMut<Point> for Grid<T> {
    fn index_mut(&mut self, p: Point) -> &mut Self::Output {
        self.get_mut(p).unwrap()
    }
}

impl<T> std::ops::Index<(isize, isize)> for Grid<T> {
    type Output = T;
    fn index(&self, p: (isize, isize)) -> &Self::Output {
        self.map_bounds(p)
            .and_then(|p| self.get(p))
            .unwrap_or_else(|| panic!("Out of Grid Bounds: {:?}", p))
    }
}
impl<T> std::ops::IndexMut<(isize, isize)> for Grid<T> {
    fn index_mut(&mut self, p: (isize, isize)) -> &mut Self::Output {
        self.map_bounds(p)
            .and_then(move |p| self.get_mut(p))
            .unwrap_or_else(|| panic!("Out of Grid Bounds: {:?}", p))
    }
}

impl<T> std::ops::Deref for Grid<T> {
    type Target = Vec<Vec<T>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T> std::ops::DerefMut for Grid<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
