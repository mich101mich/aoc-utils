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
struct Element<Id>(Id, Cost, Cost);
impl<Id: Eq> PartialOrd for Element<Id> {
    fn partial_cmp(&self, rhs: &Element<Id>) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}
impl<Id: Eq> Ord for Element<Id> {
    fn cmp(&self, rhs: &Element<Id>) -> Ordering {
        rhs.2.cmp(&self.2)
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

    let mut visited = HashMap::new();
    let mut next = BinaryHeap::new();
    next.push(Element(start, 0, 0));
    visited.insert(start, (0, start));

    let mut neighbors = vec![];

    while let Some(Element(current_id, current_cost, _)) = next.pop() {
        if current_id == goal {
            break;
        }
        match current_cost.cmp(&visited[&current_id].0) {
            Ordering::Greater => continue,
            Ordering::Equal => {}
            Ordering::Less => panic!("Went from {} to {}", current_cost, visited[&current_id].0),
        }

        neighbors.clear();
        get_all_neighbors(current_id, &mut neighbors);
        for other in neighbors.drain(..) {
            let (other_id, delta_cost) = other.into_id_cost();
            let other_cost = current_cost + delta_cost;

            let mut needs_visit = true;
            if let Some((prev_cost, prev_id)) = visited.get_mut(&other_id) {
                if *prev_cost > other_cost {
                    *prev_cost = other_cost;
                    *prev_id = current_id;
                } else {
                    needs_visit = false;
                }
            } else {
                visited.insert(other_id, (other_cost, current_id));
            }

            if needs_visit {
                let heuristic = heuristic(other_id);
                next.push(Element(other_id, other_cost, other_cost + heuristic));
            }
        }
    }

    if !visited.contains_key(&goal) {
        return None;
    }

    let steps = {
        let mut steps = vec![];
        let mut current = goal;

        while current != start {
            steps.push(current);
            let (_, prev) = visited[&current];
            current = prev;
        }
        steps.push(start);
        steps.reverse();
        steps
    };

    Some(Path::new(steps, visited[&goal].0))
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

    let mut visited = HashMap::new();
    let mut next = BinaryHeap::new();
    next.push(Element(start, 0, 0));
    visited.insert(start, (0, start));

    let mut remaining_goals = goals.iter().copied().to_set();

    let mut goal_costs = HashMap::with_capacity(goals.len());

    let mut neighbors = vec![];

    while let Some(Element(current_id, current_cost, _)) = next.pop() {
        match current_cost.cmp(&visited[&current_id].0) {
            Ordering::Greater => continue,
            Ordering::Equal => {}
            Ordering::Less => panic!("Went from {} to {}", current_cost, visited[&current_id].0),
        }

        if remaining_goals.remove(&current_id) {
            goal_costs.insert(current_id, current_cost);
            if remaining_goals.is_empty() {
                break;
            }
        }

        neighbors.clear();
        get_all_neighbors(current_id, &mut neighbors);
        for other in neighbors.drain(..) {
            let (other_id, delta_cost) = other.into_id_cost();
            let other_cost = current_cost + delta_cost;

            let mut needs_visit = true;
            if let Some((prev_cost, prev_id)) = visited.get_mut(&other_id) {
                if *prev_cost > other_cost {
                    *prev_cost = other_cost;
                    *prev_id = current_id;
                } else {
                    needs_visit = false;
                }
            } else {
                visited.insert(other_id, (other_cost, current_id));
            }

            if needs_visit {
                next.push(Element(other_id, other_cost, other_cost));
            }
        }
    }

    let mut goal_data = HashMap::with_capacity(goal_costs.len());

    for (&goal, &cost) in goal_costs.iter() {
        let steps = {
            let mut steps = vec![];
            let mut current = goal;

            while current != start {
                steps.push(current);
                let (_, prev) = visited[&current];
                current = prev;
            }
            steps.push(start);
            steps.reverse();
            steps
        };
        goal_data.insert(goal, Path::new(steps, cost));
    }

    goal_data
}
