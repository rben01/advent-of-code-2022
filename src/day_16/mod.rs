// tag::setup[]
use crate::{utils::dijkstra, Answer};
use priority_queue::PriorityQueue;
use std::{
	collections::{BTreeSet, HashMap},
	hash::Hash,
	rc::Rc,
};

fn ans_for_input(input: &str) -> Answer<i32, i32> {
	let volcano = read_input(input).expect("could not read input");
	(16, (pt1(&volcano), pt2(&volcano))).into()
}

pub fn ans() -> Answer<i32, i32> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Debug)]
struct Valve {
	name: String,
	flow_rate: i32,
	adj_valves: BTreeSet<usize>,
}

#[derive(Debug)]
struct Volcano {
	valves: HashMap<usize, Valve>,
}

fn read_input(input: &str) -> Option<Volcano> {
	let valve_re = crate::regex!(
		r"Valve (?P<valve_name>\w+) has flow rate=(?P<flow_rate>\d+); tunnels? leads? to valves? (?P<adj_valves>.*)$"
	);
	let mut valves_by_name = HashMap::new();

	for (i, line) in input.lines().enumerate() {
		let caps = valve_re.captures(line)?;
		let [valve_name, flow_rate, adj_valves] =
			["valve_name", "flow_rate", "adj_valves"].map(|name| caps.name(name));

		let valve_name = valve_name?.as_str();
		let flow_rate = flow_rate?.as_str().parse().ok()?;
		let adj_valves = adj_valves?
			.as_str()
			.split_whitespace()
			.map(|adj_valve_str| adj_valve_str.trim_end_matches(',').to_owned())
			.collect::<Vec<_>>();

		assert!(
			valves_by_name
				.insert(valve_name.to_owned(), (i, flow_rate, adj_valves),)
				.is_none(),
			"got duplicate valve {valve_name:?}"
		);
	}

	let valves = valves_by_name
		.iter()
		.map(|(name, (index, flow_rate, adj_valves))| {
			(
				*index,
				Valve {
					name: name.clone(),
					flow_rate: *flow_rate,
					adj_valves: adj_valves
						.iter()
						.map(|name| valves_by_name[name].0)
						.collect(),
				},
			)
		})
		.collect();

	Some(Volcano { valves })
}

trait StateTrait {
	fn max_value_of_node(
		&self,
		id: usize,
		valve: &Valve,
		all_valve_distances: &HashMap<usize, HashMap<usize, i32>>,
	) -> i32;

	fn max_pressure_release_theoritically_possible(
		&self,
		valves: &HashMap<usize, Valve>,
		all_valve_distances: &HashMap<usize, HashMap<usize, i32>>,
	) -> i32 {
		valves
			.iter()
			.map(|(id, valve)| self.max_value_of_node(*id, valve, all_valve_distances))
			.sum()
	}
}

struct Setup {
	start_node: usize,
	all_valve_distances: HashMap<usize, HashMap<usize, i32>>,
}
fn setup(volcano: &Volcano) -> Setup {
	let Volcano { valves } = volcano;
	let adjacencies = valves
		.iter()
		.map(|(id, Valve { adj_valves, .. })| {
			(
				*id,
				adj_valves
					.iter()
					.map(|other| (*other, 1_i32))
					.collect::<HashMap<_, _>>(),
			)
		})
		.collect::<HashMap<_, _>>();
	let all_valve_distances = adjacencies
		.keys()
		.map(|start_valve| {
			(
				*start_valve,
				dijkstra(&adjacencies, start_valve)
					.into_iter()
					.map(|(k, v)| (*k, v))
					.collect::<HashMap<_, _>>(),
			)
		})
		.collect::<HashMap<_, _>>();

	let start_node = valves
		.iter()
		.find_map(|(&id, Valve { name, .. })| (name == "AA").then_some(id))
		.expect("could not find node AA");

	Setup {
		start_node,
		all_valve_distances,
	}
}
// end::setup[]

// tag::pt1[]
fn pt1(volcano: &Volcano) -> i32 {
	#[derive(Debug, PartialEq, Eq)]
	struct State {
		location: usize,
		opened_valves: Rc<BTreeSet<usize>>,
		pressure_released: i32,
		t_left: i32,
	}

	// This is fine, we hash a subset of what's considered by PartialEq, so we won't
	// violate x == y => hash(x) == hash(y)
	#[allow(clippy::derive_hash_xor_eq)]
	impl Hash for State {
		fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
			self.location.hash(state);
			self.pressure_released.hash(state);
			self.t_left.hash(state);
		}
	}

	impl StateTrait for State {
		fn max_value_of_node(
			&self,
			id: usize,
			valve: &Valve,
			all_valve_distances: &HashMap<usize, HashMap<usize, i32>>,
		) -> i32 {
			if self.opened_valves.contains(&id) {
				0
			} else {
				let dist_to_valve = all_valve_distances[&self.location][&id];
				// The most you could get out of the valve — we'd have to move there
				// (-adj_valve_dists[id]), then open it (-1)
				0.max(valve.flow_rate * (self.t_left - dist_to_valve - 1))
			}
		}
	}

	let Setup {
		start_node,
		all_valve_distances,
	} = setup(volcano);
	let Volcano { valves } = volcano;

	// Priority is the pressure that stands to be released from this move, plus the
	// potential of all other unopened valves
	let mut pq = PriorityQueue::new();

	pq.push(
		State {
			location: start_node,
			opened_valves: Rc::new(
				valves
					.iter()
					.filter_map(|(&id, Valve { flow_rate, .. })| (*flow_rate == 0).then_some(id))
					.collect(),
			),
			pressure_released: 0,
			t_left: 30,
		},
		0,
	);

	while let Some((state, _)) = pq.pop() {
		let State {
			location,
			opened_valves: opened,
			pressure_released,
			t_left,
		} = state;

		// No time left or we've opened all valves
		if t_left == 0 || opened.len() == valves.len() {
			return pressure_released;
		}

		let t_left_after_this = t_left - 1;

		// Enqueue moves to adjacent valves
		for adj_valve in &valves[&location].adj_valves {
			let state = State {
				location: *adj_valve,
				opened_valves: Rc::clone(&opened),
				pressure_released,
				t_left: t_left_after_this,
			};
			let priority = pressure_released
				+ state.max_pressure_release_theoritically_possible(valves, &all_valve_distances);
			pq.push(state, priority);
		}

		// If can open current valve, open it
		if !opened.contains(&location) {
			let new_opened = {
				let mut set = match Rc::try_unwrap(opened) {
					Ok(sole_ref) => sole_ref,
					Err(rc) => rc.as_ref().clone(),
				};
				set.insert(location);
				Rc::new(set)
			};

			let pressure_released =
				pressure_released + valves[&location].flow_rate * t_left_after_this;
			let state = State {
				location,
				opened_valves: new_opened,
				pressure_released,
				t_left: t_left_after_this,
			};
			let priority = pressure_released
				+ state.max_pressure_release_theoritically_possible(valves, &all_valve_distances);

			pq.push(state, priority);
		}
	}

	-1
}
// end::pt1[]

// tag::pt2[]
fn pt2(volcano: &Volcano) -> i32 {
	#[derive(Debug, PartialEq, Eq)]
	struct State {
		mover_loc: usize,
		waiter_loc: usize,
		is_mid_turn: bool,
		opened_valves: Rc<BTreeSet<usize>>,
		pressure_released: i32,
		t_left: i32,
	}

	// This is fine, we hash a subset of what's considered by PartialEq, so we won't
	// violate x == y => hash(x) == hash(y)
	#[allow(clippy::derive_hash_xor_eq)]
	impl Hash for State {
		fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
			self.mover_loc.hash(state);
			self.waiter_loc.hash(state);
			self.is_mid_turn.hash(state);
			self.pressure_released.hash(state);
			self.t_left.hash(state);
		}
	}

	impl StateTrait for State {
		fn max_value_of_node(
			&self,
			id: usize,
			valve: &Valve,
			all_valve_distances: &HashMap<usize, HashMap<usize, i32>>,
		) -> i32 {
			let Self {
				mover_loc,
				waiter_loc,
				is_mid_turn,
				opened_valves,
				t_left,
				..
			} = self;
			if opened_valves.contains(&id) {
				0
			} else {
				let dists_to_this_valve = &all_valve_distances[&id];
				// the mover can start heading there now
				let mover_time_to_get_there = dists_to_this_valve[mover_loc];
				// if it's mid turn, the waiter has to wait for the clock to tick to move
				let waiter_time_to_get_there =
					dists_to_this_valve[waiter_loc] + i32::from(*is_mid_turn);
				let min_time_to_get_there = mover_time_to_get_there.min(waiter_time_to_get_there);

				// The most you could get out of the valve — the temporally nearer person
				// would have to move there (min_time_to_get_there), then open it (1)
				0.max(valve.flow_rate * (t_left - min_time_to_get_there - 1))
			}
		}
	}

	let Setup {
		start_node,
		all_valve_distances,
	} = setup(volcano);
	let Volcano { valves } = volcano;

	// Priority is the pressure that stands to be released from this move, plus the
	// potential of all other unopened valves
	let mut pq = PriorityQueue::new();

	pq.push(
		State {
			mover_loc: start_node,
			waiter_loc: start_node,
			is_mid_turn: false,
			opened_valves: Rc::new(
				valves
					.iter()
					.filter_map(|(&id, Valve { flow_rate, .. })| (*flow_rate == 0).then_some(id))
					.collect(),
			),
			pressure_released: 0,
			t_left: 26,
		},
		(0, start_node),
	);

	let mut seen = BTreeSet::new();

	while let Some((state, _)) = pq.pop() {
		let State {
			mover_loc,
			waiter_loc,
			is_mid_turn,
			opened_valves: opened,
			pressure_released,
			t_left,
		} = state;

		// No time left or we've opened all valves
		if t_left == 0 || opened.len() == valves.len() {
			return pressure_released;
		}

		let (min_loc, max_loc) = if mover_loc < waiter_loc {
			(mover_loc, waiter_loc)
		} else {
			(waiter_loc, mover_loc)
		};
		if !seen.insert((
			min_loc,
			max_loc,
			pressure_released,
			t_left,
			Rc::clone(&opened),
		)) {
			continue;
		};

		let t_left_after_both_move = t_left - 1;
		let t_left_after_mover_moves = if is_mid_turn { t_left - 1 } else { t_left };

		// Enqueue moves to adjacent valves
		for &adj_valve_loc in &valves[&mover_loc].adj_valves {
			let state = State {
				mover_loc: waiter_loc,
				waiter_loc: adj_valve_loc,
				is_mid_turn: !is_mid_turn,
				opened_valves: Rc::clone(&opened),
				pressure_released,
				t_left: t_left_after_mover_moves,
			};
			let priority = pressure_released
				+ state.max_pressure_release_theoritically_possible(valves, &all_valve_distances);

			pq.push(state, (priority, mover_loc));
		}

		// If can open current valve, enqueue opening it
		if !opened.contains(&mover_loc) {
			let pressure_released =
				pressure_released + valves[&mover_loc].flow_rate * t_left_after_both_move;
			let new_opened = {
				let mut set = match Rc::try_unwrap(opened) {
					Ok(sole_ref) => sole_ref,
					Err(rc) => rc.as_ref().clone(),
				};
				set.insert(mover_loc);
				Rc::new(set)
			};

			let state = State {
				mover_loc: waiter_loc,
				waiter_loc: mover_loc,
				is_mid_turn: !is_mid_turn,
				opened_valves: new_opened,
				pressure_released,
				t_left: t_left_after_mover_moves,
			};

			let priority = pressure_released
				+ state.max_pressure_release_theoritically_possible(valves, &all_valve_distances);

			pq.push(state, (priority, mover_loc));
		}
	}

	-1
}
// end::pt2[]

#[cfg(test)]
mod test {
	#![allow(unused_imports)]
	use super::*;
	use crate::run_tests;

	#[test]
	fn test() {
		run_tests(
			&read_input(include_str!("sample_input.txt")).unwrap(),
			(pt1, 1651),
			(pt2, 1707),
		);
		run_tests(
			&read_input(include_str!("input.txt")).unwrap(),
			(pt1, 2119),
			(pt2, 2615),
		);
	}
}
