// tag::setup[]
use crate::{read_file, Answer, Cast};
use std::{
	collections::{HashMap, HashSet},
	ops::ControlFlow,
};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

type Coord = i32;
type Ans = i32;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let elves = read_input(input);
	(23, (pt1(elves.clone()), pt2(elves))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("input.txt"))
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Pair(Coord, Coord);

trait Translates
where
	Self: Copy,
{
	/// `[d_row, d_col]`
	fn d_loc(self) -> Pair;

	/// Translate a point by `self.d_loc()`
	fn translate(self, point: Pair) -> Pair {
		let Pair(r, c) = point;
		let Pair(dr, dc) = self.d_loc();
		Pair(r + dr, c + dc)
	}
}

#[derive(Debug, Clone, Copy)]
enum Heading {
	N,
	S,
	W,
	E,
}

impl Translates for Heading {
	fn d_loc(self) -> Pair {
		use Heading::*;
		let [dr, dc] = match self {
			N => [-1, 0],
			S => [1, 0],
			W => [0, -1],
			E => [0, 1],
		};
		Pair(dr, dc)
	}
}

impl Heading {
	fn next(self) -> Self {
		use Heading::*;
		match self {
			N => S,
			S => W,
			W => E,
			E => N,
		}
	}

	fn directions(self) -> [Direction; 3] {
		use Direction::*;
		match self {
			Heading::N => [Nw, N, Ne],
			Heading::S => [Se, S, Sw],
			Heading::W => [Sw, W, Nw],
			Heading::E => [Ne, E, Se],
		}
	}

	fn attempted_headings(self) -> impl Iterator<Item = Self> {
		(0..4).scan(self, |state, _| {
			let ret = *state;
			*state = state.next();
			Some(ret)
		})
	}
}

#[derive(Debug, Clone, Copy, EnumIter)]
enum Direction {
	Nw,
	N,
	Ne,
	E,
	Se,
	S,
	Sw,
	W,
}

impl Translates for Direction {
	/// `[d_row, d_col]`
	fn d_loc(self) -> Pair {
		use Direction::*;
		let [dr, dc] = match self {
			Nw => [-1, -1],
			N => [-1, 0],
			Ne => [-1, 1],
			E => [0, 1],
			Se => [1, 1],
			S => [1, 0],
			Sw => [1, -1],
			W => [0, -1],
		};
		Pair(dr, dc)
	}
}

#[derive(Debug, Clone)]
struct Elves {
	locs: HashSet<Pair>,
	heading: Heading,
}

impl Elves {
	fn area(&self) -> Ans {
		let [mut r_min, mut c_min] = [Coord::MAX; 2];
		let [mut r_max, mut c_max] = [Coord::MIN; 2];
		for &Pair(r, c) in &self.locs {
			r_min = r_min.min(r);
			r_max = r_max.max(r);

			c_min = c_min.min(c);
			c_max = c_max.max(c);
		}
		// println!("{:?}", (r_min, r_max, c_min, c_max));
		(r_max - r_min + 1) * (c_max - c_min + 1)
	}

	fn n_empty_ground_tiles(&self) -> Ans {
		self.area() - self.locs.len().cast::<Ans>()
	}

	fn step(&mut self) -> ControlFlow<()> {
		let Self {
			locs,
			heading: curr_heading,
		} = self;

		let mut destination_counts = HashMap::new();
		let mut tentative_destinations = HashMap::new();

		for &loc in &*locs {
			let surroundings_are_empty =
				Direction::iter().all(|dir| !locs.contains(&dir.translate(loc)));

			let dest = if surroundings_are_empty {
				loc
			} else {
				curr_heading
					.attempted_headings()
					.find_map(|heading| {
						heading
							.directions()
							.into_iter()
							.all(|dir| !locs.contains(&dir.translate(loc)))
							.then(|| heading.translate(loc))
					})
					.unwrap_or(loc)
			};

			if dest != loc {
				tentative_destinations.insert(loc, dest);
			}
			*destination_counts.entry(dest).or_insert(0) += 1;
		}

		let mut did_move = false;
		for (loc, dest) in tentative_destinations {
			if destination_counts[&dest] == 1 {
				locs.remove(&loc);
				locs.insert(dest);
				did_move = true;
			}
		}

		*curr_heading = curr_heading.next();

		if did_move {
			ControlFlow::Continue(())
		} else {
			ControlFlow::Break(())
		}
	}
}

fn read_input(input: &str) -> Elves {
	let mut locs = HashSet::new();
	for (row, line) in input.lines().enumerate() {
		for (col, c) in line.chars().enumerate() {
			if c == '#' {
				locs.insert(Pair(row.cast(), col.cast()));
			}
		}
	}
	Elves {
		locs,
		heading: Heading::N,
	}
}
// end::setup[]

// tag::pt1[]
fn pt1(mut elves: Elves) -> Ans {
	for _ in 0..10 {
		let step = elves.step();
		if matches!(step, ControlFlow::Break(_)) {
			break;
		}
	}
	elves.n_empty_ground_tiles()
}
// end::pt1[]

// tag::pt2[]
fn pt2(mut elves: Elves) -> Ans {
	for i in 0.. {
		let step = elves.step();
		if matches!(step, ControlFlow::Break(_)) {
			return i + 1;
		}
	}
	unreachable!()
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
			(pt1, 110),
			(pt2, 20),
		);
		run_tests(
			read_input(&read_file!("input.txt")),
			(pt1, 3849),
			(pt2, 995),
		);
	}
}
