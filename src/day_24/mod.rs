// tag::setup[]
use crate::{read_file, Answer};
use priority_queue::PriorityQueue;
use std::{cmp::Reverse, collections::HashSet};
use strum::IntoEnumIterator;
use strum_macros::{EnumCount, EnumIter};

type Set<T> = HashSet<T>;

type Coord = usize;
type Ans = usize;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let valley = read_input(input);
	(23, (pt1(valley.clone()), pt2(valley))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("input.txt"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Pair(Coord, Coord);

impl Pair {
	fn manh_dist(self, other: Self) -> Coord {
		self.0.abs_diff(other.0) + self.1.abs_diff(other.1)
	}
}

#[derive(Debug, Clone, Copy)]
struct Range(Coord, Coord);

impl Range {
	fn lo(self) -> Coord {
		self.0
	}

	fn hi(self) -> Coord {
		self.1
	}

	fn contains(self, x: Coord) -> bool {
		(self.lo()..=self.hi()).contains(&x)
	}

	fn width(self) -> Coord {
		self.hi() - self.lo() + 1
	}
}

#[derive(Debug, Clone, Copy, EnumCount, EnumIter, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(usize)]
enum Direction {
	N,
	E,
	S,
	W,
}

#[derive(Debug, Clone, Copy)]
enum Move {
	Wait,
	Step(Direction),
}

impl Move {
	fn all_moves() -> impl Iterator<Item = Self> {
		use Move::*;
		std::iter::once(Wait).chain(Direction::iter().map(Step))
	}

	fn apply(self, loc: Pair) -> Option<Pair> {
		use Direction::*;
		use Move::*;
		let Pair(r, c) = loc;
		match self {
			Wait => Some(loc),
			Step(dir) => match dir {
				N => r.checked_sub(1).map(|r| Pair(r, c)),
				E => c.checked_add(1).map(|c| Pair(r, c)),
				S => r.checked_add(1).map(|r| Pair(r, c)),
				W => c.checked_sub(1).map(|c| Pair(r, c)),
			},
		}
	}
}

#[derive(Debug, Clone)]
struct Valley {
	start: Pair,
	end: Pair,
	rows: Range,
	cols: Range,
	blizzards: Vec<(Pair, Direction)>,
}

fn read_input(input: &str) -> Valley {
	use Direction::*;

	let mut start = None;

	let mut last_unoccupied = None;
	let mut width = 0;
	let mut height = 0;
	let mut blizzards = Vec::new();

	for (row, line) in input.lines().enumerate() {
		height += 1;
		for (col, c) in line.chars().enumerate() {
			width = width.max(col);
			let dir = match c {
				'#' | '.' => {
					if c == '.' {
						last_unoccupied = Some(Pair(row, col));
						if row == 0 {
							start = last_unoccupied;
						}
					}
					continue;
				}
				'^' => N,
				'>' => E,
				'v' => S,
				'<' => W,
				_ => panic!("unexpected char {c:?}"),
			};
			blizzards.push((Pair(row, col), dir));
		}
	}

	let start = start.unwrap();
	let end = last_unoccupied.unwrap();

	let rows = Range(1, height - 2);
	let cols = Range(1, width - 1);

	Valley {
		start,
		end,
		cols,
		rows,
		blizzards,
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Stage {
	FirstTripToEnd,
	TripBackToStart,
	SecondTripToEnd,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
	loc: Pair,
	stage: Stage,
	timestamp: usize,
}

/// A set, for each Direction, of `(timestamp, location)` of blizzards. I.e., at time
/// `timestamp`, is there a blizzard blowing to the `direction` at position `location`?
#[derive(Debug, Clone)]
struct Blizzards {
	rows: Range,
	cols: Range,
	vertical_blizzards: Vec<Set<Pair>>,
	horizontal_blizzards: Vec<Set<Pair>>,
}

impl Blizzards {
	/// Whether there is a blizzard at `loc` at time `ts`
	fn contains_at_time(&self, loc: Pair, ts: usize) -> bool {
		let Self {
			rows,
			cols,
			vertical_blizzards,
			horizontal_blizzards,
		} = &self;

		let ts_vert = ts % (rows.hi() - rows.lo() + 1);
		let ts_horiz = ts % (cols.hi() - cols.lo() + 1);

		vertical_blizzards[ts_vert].contains(&loc) || horizontal_blizzards[ts_horiz].contains(&loc)
	}
}

impl From<Valley> for Blizzards {
	fn from(valley: Valley) -> Self {
		use Direction::*;

		fn decr_row(loc: Pair, rows: Range) -> Pair {
			let Pair(mut r, c) = loc;
			r -= 1;
			if r < rows.lo() {
				r = rows.hi();
			}
			Pair(r, c)
		}

		fn incr_row(loc: Pair, rows: Range) -> Pair {
			let Pair(mut r, c) = loc;
			r += 1;
			if r > rows.hi() {
				r = rows.lo();
			}
			Pair(r, c)
		}

		fn decr_col(loc: Pair, cols: Range) -> Pair {
			let Pair(r, mut c) = loc;
			c -= 1;
			if c < cols.lo() {
				c = cols.hi();
			}
			Pair(r, c)
		}

		fn incr_col(loc: Pair, cols: Range) -> Pair {
			let Pair(r, mut c) = loc;
			c += 1;
			if c > cols.hi() {
				c = cols.lo();
			}
			Pair(r, c)
		}

		let Valley {
			rows,
			cols,
			blizzards,
			..
		} = valley;

		let mut vertical_blizzards = vec![Set::new(); rows.width()];
		let mut horizontal_blizzards = vec![Set::new(); cols.width()];

		for dir in Direction::iter() {
			// Get locations of all blizzards pointing in this direction t == 0
			let mut blizzards_now = blizzards
				.iter()
				.copied()
				.filter_map(|(loc, d)| (d == dir).then_some(loc))
				.collect::<Vec<_>>();

			let (range, blizzard_vec, new_loc_fn) = match dir {
				N => (rows, &mut vertical_blizzards, decr_row as fn(_, _) -> _),
				E => (cols, &mut horizontal_blizzards, incr_col as _),
				S => (rows, &mut vertical_blizzards, incr_row as _),
				W => (cols, &mut horizontal_blizzards, decr_col as _),
			};

			for i in 0..range.width() {
				blizzard_vec[i].extend(blizzards_now.iter().copied());
				blizzards_now = blizzards_now
					.into_iter()
					.map(|loc| new_loc_fn(loc, range))
					.collect();
			}
		}

		Self {
			rows,
			cols,
			vertical_blizzards,
			horizontal_blizzards,
		}
	}
}

/// Priority is the Reverse of the current `timestamp` plus the total (Manhattan)
/// distance left in the journey. Spending a tick to decrease distance by 1 doesn't
/// change priority; spending a tick to *not* decrease distance by 1 gives you a lower
/// priority.
fn priority(start: Pair, end: Pair, state: State) -> Reverse<usize> {
	use Stage::*;

	let State {
		loc,
		stage,
		timestamp,
	} = state;
	let net_dist = match stage {
		FirstTripToEnd => loc.manh_dist(end) + 2 * start.manh_dist(end),
		TripBackToStart => loc.manh_dist(start) + start.manh_dist(end),
		SecondTripToEnd => loc.manh_dist(end),
	};

	Reverse(net_dist + timestamp)
}

fn traverse(valley: Valley, is_done_fn: impl Fn(&State) -> bool) -> Ans {
	use Stage::*;

	let Valley {
		start,
		end,
		rows,
		cols,
		..
	} = valley;

	let blizzards = Blizzards::from(valley);

	let mut pq = PriorityQueue::new();
	{
		let initial_state = State {
			loc: start,
			stage: Stage::FirstTripToEnd,
			timestamp: 0,
		};
		pq.push(initial_state, priority(start, end, initial_state));
	}

	while let Some((
		state @ State {
			loc,
			stage,
			timestamp,
		},
		_,
	)) = pq.pop()
	{
		if is_done_fn(&state) {
			return timestamp;
		}

		let new_timestamp = timestamp + 1;
		for mv in Move::all_moves() {
			let new_loc @ Pair(new_r, new_c) = match mv.apply(loc) {
				Some(loc) => loc,
				None => continue,
			};

			if !(rows.contains(new_r) && cols.contains(new_c) || new_loc == start || new_loc == end)
				|| blizzards.contains_at_time(new_loc, new_timestamp)
			{
				continue;
			}

			let new_stage = if new_loc == end {
				if stage == FirstTripToEnd {
					TripBackToStart
				} else {
					stage
				}
			} else if new_loc == start {
				if stage == TripBackToStart {
					SecondTripToEnd
				} else {
					stage
				}
			} else {
				stage
			};

			let state = State {
				loc: new_loc,
				stage: new_stage,
				timestamp: new_timestamp,
			};
			pq.push(state, priority(start, end, state));
		}
	}

	unreachable!()
}

// end::setup[]

// tag::pt1[]
fn pt1(valley: Valley) -> Ans {
	let end = valley.end;
	traverse(valley, |&State { loc, .. }| loc == end)
}
// end::pt1[]

// tag::pt2[]
fn pt2(valley: Valley) -> Ans {
	let end = valley.end;
	traverse(valley, |&State { loc, stage, .. }| {
		loc == end && stage == Stage::SecondTripToEnd
	})
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
			read_input(&read_file!("sample_input_2.txt")),
			(pt1, 18),
			(pt2, 54),
		);
		run_tests(read_input(&read_file!("input.txt")), (pt1, 230), (pt2, 713));
	}
}
