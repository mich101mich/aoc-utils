use std::collections::{BinaryHeap, HashMap, HashSet};
use std::hash::Hash;

use super::*;

pub type Cost = usize;

#[derive(Debug, Clone)]
pub struct Path<P> {
    pub path: Vec<P>,
    pub cost: Cost,
}

impl<P> Path<P> {
    pub fn new(path: Vec<P>, cost: Cost) -> Path<P> {
        Path { path, cost }
    }
    pub fn append(&mut self, node: P, cost: Cost) -> &mut Self {
        self.path.push(node);
        self.cost += cost;
        self
    }
}

use std::ops::*;

impl<P> Index<usize> for Path<P> {
    type Output = P;
    fn index(&self, index: usize) -> &P {
        &self.path[index]
    }
}

pub trait IntoIdCost<Id> {
    fn into_id_cost(self) -> (Id, Cost);
}
impl<Id> IntoIdCost<Id> for Id {
    fn into_id_cost(self) -> (Id, Cost) {
        (self, 1)
    }
}
impl<Id> IntoIdCost<Id> for (Id, Cost) {
    fn into_id_cost(self) -> (Id, Cost) {
        self
    }
}
impl<'a, Id: Copy> IntoIdCost<Id> for &'a (Id, Cost) {
    fn into_id_cost(self) -> (Id, Cost) {
        *self
    }
}
impl<'a, 'b, Id: Copy> IntoIdCost<Id> for (&'a Id, &'b Cost) {
    fn into_id_cost(self) -> (Id, Cost) {
        (*self.0, *self.1)
    }
}
impl<'a, Id: Copy> IntoIdCost<Id> for (&'a Id, Cost) {
    fn into_id_cost(self) -> (Id, Cost) {
        (*self.0, self.1)
    }
}
impl<'b, Id> IntoIdCost<Id> for (Id, &'b Cost) {
    fn into_id_cost(self) -> (Id, Cost) {
        (self.0, *self.1)
    }
}

#[derive(PartialEq, Eq)]
pub struct Element<Id> {
    pub id: Id,
    pub cost: Cost,
    pub total_heuristic: Cost,
}
impl<Id> Element<Id> {
    pub fn new(id: Id, cost: Cost, heuristic: Cost) -> Element<Id> {
        Element {
            id,
            cost,
            total_heuristic: cost + heuristic,
        }
    }
}
impl<Id: Eq> PartialOrd for Element<Id> {
    fn partial_cmp(&self, rhs: &Element<Id>) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}
impl<Id: Eq> Ord for Element<Id> {
    fn cmp(&self, rhs: &Element<Id>) -> Ordering {
        rhs.total_heuristic.cmp(&self.total_heuristic) // reverse order for max-heap
    }
}

pub struct Visited<Id>(HashMap<Id, (Cost, Id)>);

impl<Id> Visited<Id>
where
    Id: Copy + Eq + Hash,
{
    pub fn new() -> Self {
        Visited(HashMap::new())
    }
    pub fn update(&mut self, id: Id, cost: Cost, prev: Id) -> bool {
        match self.0.entry(id) {
            Entry::Occupied(mut entry) => {
                if entry.get().0 <= cost {
                    false
                } else {
                    *entry.get_mut() = (cost, prev);
                    true
                }
            }
            Entry::Vacant(entry) => {
                entry.insert((cost, prev));
                true
            }
        }
    }
    pub fn get(&self, id: Id) -> Option<(Cost, Id)> {
        self.0.get(&id).copied()
    }
    pub fn cost(&self, id: Id) -> Option<Cost> {
        self.0.get(&id).map(|(cost, _)| *cost)
    }
    pub fn prev(&self, id: Id) -> Option<Id> {
        self.0.get(&id).map(|(_, prev)| *prev)
    }

    pub fn path(&self, start: Id, goal: Id) -> Option<Path<Id>> {
        let cost = self.cost(goal)?;

        let mut steps = vec![];
        let mut current = goal;

        while current != start {
            steps.push(current);
            current = self.prev(current)?;
        }
        steps.push(start);
        steps.reverse();

        Some(Path::new(steps, cost))
    }
}

pub fn a_star_search<Id, IdCost>(
    mut get_all_neighbors: impl FnMut(Id, &mut Vec<IdCost>),
    start: Id,
    goal: Id,
    mut heuristic: impl FnMut(Id) -> Cost,
) -> Option<Path<Id>>
where
    Id: Copy + Eq + Hash,
    IdCost: IntoIdCost<Id>,
{
    if start == goal {
        return Some(Path::new(vec![start, start], 0));
    }

    let mut visited = Visited::new();
    visited.update(start, 0, start);

    let mut next = BinaryHeap::new();
    next.push(Element::new(start, 0, heuristic(start)));

    let mut neighbors = vec![];

    while let Some(current) = next.pop() {
        match current.cost.cmp(&visited.cost(current.id).unwrap()) {
            Ordering::Greater => continue,
            Ordering::Equal => {}
            Ordering::Less => unreachable!("invalid arrangement of costs"),
        }

        if current.id == goal {
            break;
        }

        neighbors.clear();
        get_all_neighbors(current.id, &mut neighbors);
        for other in neighbors.drain(..) {
            let (other_id, delta_cost) = other.into_id_cost();
            let other_cost = current.cost + delta_cost;

            if visited.update(other_id, other_cost, current.id) {
                next.push(Element::new(other_id, other_cost, heuristic(other_id)));
            }
        }
    }

    visited.path(start, goal)
}

pub fn dijkstra_search<Id, IdCost>(
    mut get_all_neighbors: impl FnMut(Id, &mut Vec<IdCost>),
    start: Id,
    goals: &[Id],
) -> HashMap<Id, Path<Id>>
where
    Id: Copy + Eq + Hash,
    IdCost: IntoIdCost<Id>,
{
    if goals.is_empty() {
        return HashMap::new();
    }

    let mut visited = Visited::new();
    visited.update(start, 0, start);

    let mut next = BinaryHeap::new();
    next.push(Element::new(start, 0, 0));

    let mut remaining_goals = goals.iter().copied().to_set();

    let mut neighbors = vec![];

    while let Some(current) = next.pop() {
        match current.cost.cmp(&visited.cost(current.id).unwrap()) {
            Ordering::Greater => continue,
            Ordering::Equal => {}
            Ordering::Less => unreachable!("invalid arrangement of costs"),
        }

        if remaining_goals.remove(&current.id) && remaining_goals.is_empty() {
            break;
        }

        neighbors.clear();
        get_all_neighbors(current.id, &mut neighbors);
        for other in neighbors.drain(..) {
            let (other_id, delta_cost) = other.into_id_cost();
            let other_cost = current.cost + delta_cost;

            if visited.update(other_id, other_cost, current.id) {
                next.push(Element::new(other_id, other_cost, 0));
            }
        }
    }

    if remaining_goals.len() == goals.len() {
        return HashMap::new();
    }

    let mut goal_data = HashMap::with_capacity(goals.len() - remaining_goals.len());

    for &goal in goals {
        if let Some(path) = visited.path(start, goal) {
            goal_data.insert(goal, path);
        }
    }

    goal_data
}

/// A version of Dijkstra's algorithm with manual termination instead of a goal.
///
/// ## Arguments
/// - `find_goal`: A function to check if a position is a goal, or to get its neighbors.
///   - Arguments: `pos`: current position, `out`: a vector to write neighbors to
///   - Returns: `true` if the search should terminate
/// - `start`: The starting position.
pub fn open_dijkstra<Id, IdCost, F>(mut find_goal: F, start: Id) -> Option<Path<Id>>
where
    Id: Copy + Eq + Hash,
    IdCost: IntoIdCost<Id>,
    F: FnMut(Id, &mut Vec<IdCost>) -> bool,
{
    let mut visited = Visited::new();
    visited.update(start, 0, start);

    let mut next = BinaryHeap::new();
    next.push(Element::new(start, 0, 0));

    let mut neighbors = vec![];

    while let Some(current) = next.pop() {
        match current.cost.cmp(&visited.cost(current.id).unwrap()) {
            Ordering::Greater => continue,
            Ordering::Equal => {}
            Ordering::Less => unreachable!("invalid arrangement of costs"),
        }

        neighbors.clear();
        if find_goal(current.id, &mut neighbors) {
            return visited.path(start, current.id);
        }

        for other in neighbors.drain(..) {
            let (other_id, delta_cost) = other.into_id_cost();
            let other_cost = current.cost + delta_cost;

            if visited.update(other_id, other_cost, current.id) {
                next.push(Element::new(other_id, other_cost, 0));
            }
        }
    }

    None
}
