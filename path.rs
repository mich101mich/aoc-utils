use std::collections::{HashMap, HashSet};

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

pub fn ordered_insert<T, V, F>(vector: &mut Vec<T>, element: T, get_value: F)
where
    T: std::fmt::Debug,
    V: Ord,
    F: Fn(&T) -> V,
{
    let value = get_value(&element);
    let mut a = 0;
    let mut b = vector.len();
    while b - a > 1 {
        let mid = (a + b) / 2;
        if get_value(&vector[mid]) >= value {
            a = mid;
        } else {
            b = mid;
        }
    }
    vector.insert(b, element);
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

pub fn a_star_search<Id, GetNeighbors, NeighborIter, NeighborReturn, Heuristic>(
    mut get_all_neighbors: GetNeighbors,
    start: Id,
    goal: Id,
    mut heuristic: Heuristic,
) -> Option<Path<Id>>
where
    Id: Copy + std::cmp::Eq + std::hash::Hash + std::fmt::Debug,
    GetNeighbors: FnMut(Id) -> NeighborIter,
    NeighborIter: Iterator<Item = NeighborReturn>,
    NeighborReturn: IntoIdCost<Id>,
    Heuristic: FnMut(Id) -> Cost,
{
    if start == goal {
        return Some(Path::new(vec![start, start], 0));
    }
    let mut visited = HashMap::new();
    let mut next = vec![(start, 0)];
    visited.insert(start, (0, start));

    'search: while let Some((current_id, _)) = next.pop() {
        if current_id == goal {
            break 'search;
        }
        let current_cost = visited[&current_id].0;

        for other in get_all_neighbors(current_id) {
            let (other_id, delta_cost) = other.into_id_cost();
            let other_cost = current_cost + delta_cost;

            let heuristic = heuristic(other_id);

            if let Some(&(prev_cost, _)) = visited.get(&other_id) {
                if prev_cost > other_cost {
                    next.retain(|&(id, _)| id != other_id);
                }
            }

            if !visited.contains_key(&other_id) || visited[&other_id].0 > other_cost {
                ordered_insert(
                    &mut next,
                    (other_id, other_cost + heuristic),
                    |&(_, cost)| cost,
                );
                visited.insert(other_id, (other_cost, current_id));
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

pub fn dijkstra_search<Id, GetNeighbors, NeighborIter, NeighborReturn>(
    mut get_all_neighbors: GetNeighbors,
    start: Id,
    goals: &[Id],
) -> HashMap<Id, Path<Id>>
where
    Id: Copy + ::std::cmp::Eq + ::std::hash::Hash + ::std::fmt::Debug,
    GetNeighbors: FnMut(Id) -> NeighborIter,
    NeighborIter: Iterator<Item = NeighborReturn>,
    NeighborReturn: IntoIdCost<Id>,
{
    let mut visited = ::std::collections::HashMap::new();
    let mut next = vec![(start, 0)];
    visited.insert(start, (0, start));

    let mut remaining_goals = goals.to_vec();

    let mut goal_costs = HashMap::with_capacity(goals.len());

    while let Some((current_id, _)) = next.pop() {
        let cost = visited[&current_id].0;

        for &goal_id in remaining_goals.iter() {
            if current_id == goal_id {
                goal_costs.insert(goal_id, cost);
            }
        }
        remaining_goals.retain(|&id| id != current_id);
        if remaining_goals.is_empty() {
            break;
        }

        for other in get_all_neighbors(current_id) {
            let (other_id, delta_cost) = other.into_id_cost();
            let other_cost = cost + delta_cost;

            if let Some(&(prev_cost, _)) = visited.get(&other_id) {
                if prev_cost > other_cost {
                    next.retain(|&(id, _)| id != other_id);
                }
            }

            if !visited.contains_key(&other_id) || visited[&other_id].0 > other_cost {
                ordered_insert(&mut next, (other_id, other_cost), |&(_, cost)| cost);
                visited.insert(other_id, (other_cost, current_id));
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
