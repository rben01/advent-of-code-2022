use std::collections::BTreeSet;

// tag::setup[]
use crate::Answer;

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let mat = read_input(input).expect("couldn't read input");
	(9, (pt1(&mat), pt2(&mat))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Position(i32, i32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Displacement(i32, i32);

impl std::ops::Sub<Position> for Position {
	type Output = Displacement;
	fn sub(self, rhs: Position) -> Self::Output {
		Displacement(self.0 - rhs.0, self.1 - rhs.1)
	}
}

impl std::ops::Add<Displacement> for Position {
	type Output = Position;
	fn add(self, rhs: Displacement) -> Self::Output {
		Position(self.0 + rhs.0, self.1 + rhs.1)
	}
}

impl std::ops::AddAssign<Displacement> for Position {
	fn add_assign(&mut self, rhs: Displacement) {
		*self = *self + rhs;
	}
}

#[derive(Debug, Clone, Copy)]
enum Direction {
	Up,
	Down,
	Left,
	Right,
}

impl Direction {
	fn displacement(self) -> Displacement {
		use Direction::*;
		let (dx, dy) = match self {
			Up => (0, 1),
			Down => (0, -1),
			Left => (-1, 0),
			Right => (1, 0),
		};
		Displacement(dx, dy)
	}
}

impl TryFrom<&str> for Direction {
	type Error = String;
	fn try_from(value: &str) -> Result<Self, Self::Error> {
		use Direction::*;
		let dir = match value {
			"U" => Up,
			"D" => Down,
			"L" => Left,
			"R" => Right,
			_ => return Err(format!("invalid direction {value:?}")),
		};
		Ok(dir)
	}
}

#[derive(Debug, Clone, Copy)]
struct Step {
	distance: usize,
	direction: Direction,
}

fn read_input(input: &str) -> Option<Vec<Step>> {
	let mut directions = Vec::new();

	for line in input.lines() {
		let mut comps = line.split_whitespace();
		let direction = Direction::try_from(comps.next()?).ok()?;
		let distance = comps.next()?.parse().ok()?;
		directions.push(Step {
			distance,
			direction,
		});
	}

	Some(directions)
}

fn tail_displacement(head_pos: Position, tail_pos: Position) -> Displacement {
	let Displacement(dx, dy) = head_pos - tail_pos;

	let d_tail = match (dx, dy) {
		(-1 | 0 | 1, -1 | 0 | 1) => (0, 0),
		(2, 0) => (1, 0),
		(-2, 0) => (-1, 0),
		(0, 2) => (0, 1),
		(0, -2) => (0, -1),
		(1 | 2, 2) | (2, 1) => (1, 1),
		(-1 | -2, 2) | (-2, 1) => (-1, 1),
		(-1 | -2, -2) | (-2, -1) => (-1, -1),
		(1 | 2, -2) | (2, -1) => (1, -1),
		_ => panic!(
			"invalid state: head={head_pos:?}, tail={tail_pos:?}, disp={:?}",
			(dx, dy)
		),
	};

	Displacement(d_tail.0, d_tail.1)
}

fn move_rope<const N_KNOTS: usize>(steps: &[Step]) -> usize {
	let mut seen_locs = BTreeSet::new();

	// knots[0] is the head, knots[N_KNOTS-1] is the tail
	let mut knots = [Position(0, 0); N_KNOTS];

	for &step in steps {
		for _ in 0..step.distance {
			knots[0] += step.direction.displacement();
			let mut headward_knot = knots[0];

			for tailward_knot in &mut knots[1..] {
				*tailward_knot += tail_displacement(headward_knot, *tailward_knot);
				headward_knot = *tailward_knot;
			}
			seen_locs.insert(knots[N_KNOTS - 1]);
		}
	}

	seen_locs.len()
}
// end::setup[]

// tag::pt1[]
fn pt1(steps: impl AsRef<[Step]>) -> usize {
	let steps = steps.as_ref();
	move_rope::<2>(steps)
}
// end::pt1[]

// tag::pt2[]
fn pt2(steps: impl AsRef<[Step]>) -> usize {
	let steps = steps.as_ref();
	move_rope::<10>(steps)
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
			(pt1, 13),
			(pt2, 1),
		);
		run_tests(
			&read_input(include_str!("input.txt")).unwrap(),
			(pt1, 6256),
			(pt2, 2665),
		);
	}
}
