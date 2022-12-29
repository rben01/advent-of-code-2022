#![allow(dead_code)]
use num::{CheckedAdd, CheckedSub, Num, One, Zero};
use priority_queue::PriorityQueue;
use std::{
	borrow::Borrow,
	cell::RefCell,
	cmp::{Ord, Ordering},
	collections::HashMap,
	fmt::Debug,
	hash::Hash,
	ops::RangeBounds,
	rc::Rc,
};

pub type RcRc<T> = Rc<RefCell<T>>;

pub fn into_rc_rc<T>(x: T) -> RcRc<T> {
	Rc::new(RefCell::new(x))
}

pub fn get_nsew_adjacent<
	X: Copy + Num + Zero + One + CheckedAdd + CheckedSub + Ord + std::fmt::Debug,
	Y: Copy + Num + Zero + One + CheckedAdd + CheckedSub + Ord + std::fmt::Debug,
	XRangeT: RangeBounds<X> + std::fmt::Debug,
	YRangeT: RangeBounds<Y> + std::fmt::Debug,
>(
	pos_xy: (X, Y),
	x_bounds: XRangeT,
	y_bounds: YRangeT,
) -> [Option<(X, Y)>; 4] {
	enum D {
		MinusOne,
		Zero,
		PlusOne,
	}
	use D::*;

	let (x, y) = pos_xy;

	[
		(MinusOne, Zero),
		(PlusOne, Zero),
		(Zero, MinusOne),
		(Zero, PlusOne),
	]
	.map(|(dx, dy)| {
		let new_x = match dx {
			MinusOne => x.checked_sub(&X::one())?,
			Zero => x,
			PlusOne => x.checked_add(&X::one())?,
		};
		let new_y = match dy {
			MinusOne => y.checked_sub(&Y::one())?,
			Zero => y,
			PlusOne => y.checked_add(&Y::one())?,
		};

		(x_bounds.contains(&new_x) && y_bounds.contains(&new_y)).then_some((new_x, new_y))
	})
}

pub fn get_xyz_adjacent<
	X: Copy + Num + Zero + One + CheckedAdd + CheckedSub + Ord + Debug,
	Y: Copy + Num + Zero + One + CheckedAdd + CheckedSub + Ord + Debug,
	Z: Copy + Num + Zero + One + CheckedAdd + CheckedSub + Ord + Debug,
	XRangeT: RangeBounds<X> + Debug,
	YRangeT: RangeBounds<Y> + Debug,
	ZRangeT: RangeBounds<Z> + Debug,
>(
	pos_xyz: impl Into<(X, Y, Z)>,
	x_bounds: XRangeT,
	y_bounds: YRangeT,
	z_bounds: ZRangeT,
) -> [Option<(X, Y, Z)>; 6] {
	enum D {
		MinusOne,
		Zero,
		PlusOne,
	}
	use D::*;

	let (x, y, z) = pos_xyz.into();

	[
		(MinusOne, Zero, Zero),
		(PlusOne, Zero, Zero),
		(Zero, MinusOne, Zero),
		(Zero, PlusOne, Zero),
		(Zero, Zero, MinusOne),
		(Zero, Zero, PlusOne),
	]
	.map(|(dx, dy, dz)| {
		let new_x = match dx {
			MinusOne => x.checked_sub(&X::one())?,
			Zero => x,
			PlusOne => x.checked_add(&X::one())?,
		};
		let new_y = match dy {
			MinusOne => y.checked_sub(&Y::one())?,
			Zero => y,
			PlusOne => y.checked_add(&Y::one())?,
		};
		let new_z = match dz {
			MinusOne => z.checked_sub(&Z::one())?,
			Zero => z,
			PlusOne => z.checked_add(&Z::one())?,
		};

		(x_bounds.contains(&new_x) && y_bounds.contains(&new_y) && z_bounds.contains(&new_z))
			.then_some((new_x, new_y, new_z))
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
			match (&self.dist, &other.dist) {
				(None, _) => Ordering::Less,
				(_, None) => Ordering::Greater,
				(Some(my_dist), Some(other_dist)) => other_dist.cmp(my_dist),
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
			unvisited.push_increase(adj_node, Priority::new_from_dist(*new_dist));
		}
	}

	dists
}
