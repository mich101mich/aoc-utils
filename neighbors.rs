#![allow(unused)]
//! A crate with the most common Neighborhoods

use super::*;

/// Defines how a Path can move along the Grid.
///
/// Different Scenarios may have different constraints as to how a Path may be formed.
/// For example if Agents can only move along the 4 cardinal directions, any Paths generated should
/// reflect that by only containing those steps.
///
/// This Trait is a generalized solution to that problem. It provides a function to query all
/// neighboring Points of an existing Point and a Heuristic for how long it might take to reach
/// a goal from a Point.
///
/// The most common implementations of this Trait are already provided by this Module:
/// - [`ManhattanNeighborhood`] for Agents that can move
/// up, down, left or right
/// - [`MooreNeighborhood`] for Agents that can move
/// up, down, left, right, as well as the 4 diagonals (up-right, ...)
pub trait Neighborhood: Clone + std::fmt::Debug {
    /// Provides a list of Neighbors of a Point
    ///
    /// Note that it is not necessary to check weather the Tile at a Point is solid or not.
    /// That check is done later.
    fn get_all_neighbors(&self, point: Point) -> Box<dyn Iterator<Item = Point>>;
    /// Gives a Heuristic for how long it takes to reach `goal` from `point`.
    ///
    /// This is usually the Distance between the two Points in the Metric of your Neighborhood.
    ///
    /// If there is no proper way of calculation how long it takes, simply return 0. This will
    /// increase the time it takes to calculate the Path, but at least it will always be correct.
    fn heuristic(&self, point: Point, goal: Point) -> usize;
}

/// A Neighborhood for Agents moving along the 4 cardinal directions.
///
/// Also known as [Von Neumann Neighborhood](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood),
/// Manhattan Metric or [Taxicab Geometry](https://en.wikipedia.org/wiki/Taxicab_geometry).
///
/// ```no_code
/// A: Agent, o: reachable in one step
///   o
///   |
/// o-A-o
///   |
///   o
/// ```
#[derive(Clone, Copy, Debug)]
pub struct ManhattanNeighborhood {
    width: usize,
    height: usize,
}

impl ManhattanNeighborhood {
    /// Creates a new ManhattanNeighborhood.
    ///
    /// `width` and `height` are the size of the Grid to move on.
    pub fn new(width: usize, height: usize) -> ManhattanNeighborhood {
        ManhattanNeighborhood { width, height }
    }
}

impl Neighborhood for ManhattanNeighborhood {
    fn get_all_neighbors(&self, point: Point) -> Box<dyn Iterator<Item = Point>> {
        let bounds = p2(self.width, self.height);
        Box::new(super::Dir::all().filter_map(move |d| d.bounded_add(point, bounds)))
    }
    fn heuristic(&self, point: Point, goal: Point) -> usize {
        let diff_0 = if goal.x > point.x {
            goal.x - point.x
        } else {
            point.x - goal.x
        };
        let diff_1 = if goal.y > point.y {
            goal.y - point.y
        } else {
            point.y - goal.y
        };
        diff_0 + diff_1
    }
}

/// A Neighborhood for Agents moving along the 4 cardinal directions and the 4 diagonals.
///
/// Also known as [Moore Neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood),
/// [Maximum Metric](https://en.wikipedia.org/wiki/Chebyshev_distance) or Chebyshev Metric.
///
/// ```no_code
/// A: Agent, o: reachable in one step
/// o o o
///  \|/
/// o-A-o
///  /|\
/// o o o
/// ```
#[derive(Clone, Copy, Debug)]
pub struct MooreNeighborhood {
    width: usize,
    height: usize,
}

impl MooreNeighborhood {
    /// Creates a new MooreNeighborhood.
    ///
    /// `width` and `height` are the size of the Grid to move on.
    pub fn new(width: usize, height: usize) -> MooreNeighborhood {
        MooreNeighborhood { width, height }
    }
}

impl Neighborhood for MooreNeighborhood {
    fn get_all_neighbors(&self, point: Point) -> Box<dyn Iterator<Item = Point>> {
        let (width, height) = (self.width, self.height);

        let iter = [
            (0isize, -1isize),
            (1, -1),
            (1, 0),
            (1, 1),
            (0, 1),
            (-1, 1),
            (-1, 0),
            (-1, -1),
        ]
        .iter()
        .filter_map(move |(dx, dy)| {
            Some(p2(
                point.x.checked_add_signed(*dx)?,
                point.y.checked_add_signed(*dy)?,
            ))
        })
        .filter(move |p| p.x < width && p.y < height);

        Box::new(iter)
    }
    fn heuristic(&self, point: Point, goal: Point) -> usize {
        let diff_0 = if goal.x > point.x {
            goal.x - point.x
        } else {
            point.x - goal.x
        };
        let diff_1 = if goal.y > point.y {
            goal.y - point.y
        } else {
            point.y - goal.y
        };
        diff_0.max(diff_1)
    }
}
