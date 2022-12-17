#![allow(dead_code)]
use num::{Num, Zero};
use priority_queue::PriorityQueue;
use std::{
	borrow::Borrow,
	cell::RefCell,
	cmp::{Ord, Ordering},
	collections::HashMap,
	hash::Hash,
	rc::Rc,
};

pub type RcRc<T> = Rc<RefCell<T>>;

pub fn into_rc_rc<T>(x: T) -> RcRc<T> {
	Rc::new(RefCell::new(x))
}

pub fn get_nsew_adjacent(
	pos: (usize, usize),
	row_bounds: (usize, usize),
	col_bounds: (usize, usize),
) -> [Option<(usize, usize)>; 4] {
	enum D {
		MinusOne,
		Zero,
		PlusOne,
	}
	use D::*;

	let (row, col) = pos;
	let (min_row, max_row) = row_bounds;
	let (min_col, max_col) = col_bounds;

	[
		(MinusOne, Zero),
		(PlusOne, Zero),
		(Zero, MinusOne),
		(Zero, PlusOne),
	]
	.map(|(dr, dc)| {
		let new_row = match dr {
			MinusOne => row.checked_sub(1)?,
			Zero => row,
			PlusOne => row + 1,
		};
		let new_col = match dc {
			MinusOne => col.checked_sub(1)?,
			Zero => col,
			PlusOne => col + 1,
		};

		((min_row..=max_row).contains(&new_row) && (min_col..=max_col).contains(&new_col))
			.then_some((new_row, new_col))
	})
}

pub fn dijkstra<'a, K, Q, N>(
	adjacencies: &'a HashMap<K, HashMap<K, N>>,
	start: &'_ K,
) -> HashMap<&'a Q, N>
where
	K: Eq + Hash + Borrow<Q>,
	Q: ?Sized + Eq + Hash,
	N: Num + Zero + Ord + Copy,
{
	#[derive(Debug, Eq, PartialEq)]
	struct Priority<N_: Ord> {
		dist: Option<N_>,
	}

	impl<N_: Ord> Priority<N_> {
		fn new_min() -> Self {
			Self { dist: None }
		}

		fn new_from_dist(dist: N_) -> Self {
			Self { dist: Some(dist) }
		}
	}

	impl<N_: Ord> PartialOrd for Priority<N_> {
		fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
			Some(self.cmp(other))
		}
	}

	impl<N_: Ord> Ord for Priority<N_> {
		fn cmp(&self, other: &Self) -> Ordering {
			match &self.dist {
				None => Ordering::Less,
				Some(my_dist) => match &other.dist {
					None => Ordering::Greater,
					Some(other_dist) => other_dist.cmp(my_dist),
				},
			}
		}
	}

	let mut unvisited = adjacencies
		.keys()
		.map(|k| (k, Priority::new_min()))
		.collect::<PriorityQueue<_, _>>();
	unvisited
		.change_priority(start, Priority::new_from_dist(N::zero()))
		.unwrap();

	let mut dists = HashMap::new();

	while let Some((node, from_start_to_here_priority)) = unvisited.pop() {
		let dist_from_start_to_here = from_start_to_here_priority.dist.unwrap();
		dists.insert(node.borrow(), dist_from_start_to_here);

		for (adj_node, &dist_from_here_to_adj_node) in &adjacencies[node.borrow()] {
			if unvisited.get(adj_node).is_none() {
				continue;
			}

			let tentative_dist = dist_from_start_to_here + dist_from_here_to_adj_node;
			let new_dist = dists.entry(adj_node.borrow()).or_insert(tentative_dist);
			*new_dist = tentative_dist.min(*new_dist);
			unvisited.push_increase(adj_node, Priority::new_from_dist(tentative_dist));
		}
	}

	dists
}
