// tag::setup[]
use crate::{utils::get_nsew_adjacent, Answer};

type Set<K> = std::collections::HashSet<K>;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let chamber = read_input(input).expect("could not read input");
	(17, (pt1(&chamber), pt2(&chamber))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(include_str!("input.txt"))
}

type Ans = i64;
type X = i32;
type Y = i64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Point(X, Y);

impl Point {
	const fn x(self) -> X {
		self.0
	}
	const fn y(self) -> Y {
		self.1
	}
	const fn translated(self, dx: X, dy: Y) -> Point {
		Point(self.0 + dx, self.1 + dy)
	}
	const fn as_row_col_tuple(self) -> (Y, X) {
		(self.y(), self.x())
	}
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum RockKind {
	FourHorizontal,
	FivePlusSign,
	FiveBackwardsL,
	FourVertical,
	FourSquare,
}

impl RockKind {
	const fn occupied_rel(self) -> &'static [Point] {
		use Point as P;
		use RockKind::*;
		match self {
			FourHorizontal => [P(0, 0), P(1, 0), P(2, 0), P(3, 0)].as_slice(),
			FivePlusSign => [P(1, 0), P(0, 1), P(1, 1), P(2, 1), P(1, 2)].as_slice(),
			FiveBackwardsL => [P(0, 0), P(1, 0), P(2, 0), P(2, 1), P(2, 2)].as_slice(),
			FourVertical => [P(0, 0), P(0, 1), P(0, 2), P(0, 3)].as_slice(),
			FourSquare => [P(0, 0), P(1, 0), P(0, 1), P(1, 1)].as_slice(),
		}
	}

	const fn left_edge_rel(self) -> &'static [Point] {
		use Point as P;
		use RockKind::*;
		match self {
			FourHorizontal => [P(0, 0)].as_slice(),
			FivePlusSign => [P(1, 0), P(0, 1), P(1, 2)].as_slice(),
			FiveBackwardsL => [P(0, 0), P(2, 1), P(2, 2)].as_slice(),
			FourVertical => [P(0, 0), P(0, 1), P(0, 2), P(0, 3)].as_slice(),
			FourSquare => [P(0, 0), P(0, 1)].as_slice(),
		}
	}

	const fn right_edge_rel(self) -> &'static [Point] {
		use Point as P;
		use RockKind::*;
		match self {
			FourHorizontal => [P(3, 0)].as_slice(),
			FivePlusSign => [P(1, 0), P(2, 1), P(1, 2)].as_slice(),
			FiveBackwardsL => [P(2, 0), P(2, 1), P(2, 2)].as_slice(),
			FourVertical => [P(0, 0), P(0, 1), P(0, 2), P(0, 3)].as_slice(),
			FourSquare => [P(1, 0), P(1, 1)].as_slice(),
		}
	}

	const fn bottom_edge_rel(self) -> &'static [Point] {
		use Point as P;
		use RockKind::*;
		match self {
			FourHorizontal => [P(0, 0), P(1, 0), P(2, 0), P(3, 0)].as_slice(),
			FivePlusSign => [P(1, 0), P(0, 1), P(2, 1)].as_slice(),
			FiveBackwardsL => [P(0, 0), P(1, 0), P(2, 0)].as_slice(),
			FourVertical => [P(0, 0)].as_slice(),
			FourSquare => [P(0, 0), P(1, 0)].as_slice(),
		}
	}

	const fn height(self) -> Y {
		use RockKind::*;
		match self {
			FourHorizontal => 1,
			FivePlusSign | FiveBackwardsL => 3,
			FourVertical => 4,
			FourSquare => 2,
		}
	}
}

#[derive(Debug, Clone, Copy)]
struct Rock {
	kind: RockKind,
	lower_left: Point,
}

impl Rock {
	fn new(kind: RockKind, lower_left: Point) -> Self {
		Self { kind, lower_left }
	}

	fn rel_to_abs(self, points: &'_ [Point]) -> impl '_ + Iterator<Item = Point> {
		let Point(dx, dy) = self.lower_left;
		points.iter().map(move |p| p.translated(dx, dy))
	}

	fn occupied(self) -> impl Iterator<Item = Point> {
		self.rel_to_abs(self.kind.occupied_rel())
	}

	fn left_edge(self) -> impl Iterator<Item = Point> {
		self.rel_to_abs(self.kind.left_edge_rel())
	}

	fn right_edge(self) -> impl Iterator<Item = Point> {
		self.rel_to_abs(self.kind.right_edge_rel())
	}

	fn bottom_edge(self) -> impl Iterator<Item = Point> {
		self.rel_to_abs(self.kind.bottom_edge_rel())
	}
}

#[derive(Debug, Clone, Copy)]
enum Jet {
	Left,
	Right,
}

impl Jet {
	fn dx(self) -> X {
		use Jet::*;
		match self {
			Left => -1,
			Right => 1,
		}
	}
}

impl TryFrom<char> for Jet {
	type Error = String;
	fn try_from(c: char) -> Result<Self, Self::Error> {
		Ok(match c {
			'<' => Self::Left,
			'>' => Self::Right,
			_ => return Err(format!("could not make Jet from char {c:?}")),
		})
	}
}

#[derive(Debug)]
struct Chamber {
	rock_seq: [RockKind; 5],
	jet_seq: Vec<Jet>,
}

impl Chamber {
	fn new(gas_jet_seq: Vec<Jet>) -> Self {
		use RockKind::*;
		Self {
			rock_seq: [
				FourHorizontal,
				FivePlusSign,
				FiveBackwardsL,
				FourVertical,
				FourSquare,
			],
			jet_seq: gas_jet_seq,
		}
	}

	fn rock_iter(&self) -> RockIter {
		RockIter {
			chamber: self,
			index: 0,
		}
	}

	fn jet_iter(&self) -> JetIter {
		JetIter {
			chamber: self,
			index: 0,
		}
	}
}

struct RockIter<'a> {
	chamber: &'a Chamber,
	index: usize,
}

impl RockIter<'_> {
	fn len(&self) -> usize {
		self.chamber.rock_seq.len()
	}
}

impl Iterator for RockIter<'_> {
	type Item = RockKind;
	fn next(&mut self) -> Option<Self::Item> {
		let Self {
			chamber: Chamber { rock_seq, .. },
			index,
		} = self;

		let rock = rock_seq[*index];
		*index = (*index + 1) % rock_seq.len();

		Some(rock)
	}
}

struct JetIter<'a> {
	chamber: &'a Chamber,
	index: usize,
}

impl JetIter<'_> {
	fn len(self) -> usize {
		self.chamber.jet_seq.len()
	}
}

impl Iterator for JetIter<'_> {
	type Item = Jet;
	fn next(&mut self) -> Option<Self::Item> {
		let Self {
			chamber: Chamber { jet_seq, .. },
			index,
		} = self;

		let jet = jet_seq[*index];
		*index = (*index + 1) % jet_seq.len();

		Some(jet)
	}
}

fn read_input(input: &str) -> Result<Chamber, String> {
	let gas_jet_dirs = input
		.trim()
		.chars()
		.map(Jet::try_from)
		.collect::<Result<_, _>>()?;

	Ok(Chamber::new(gas_jet_dirs))
}

fn solve(chamber: &Chamber, n: usize) -> Ans {
	fn is_blocked(rock_locs: &Set<Point>, point: Point) -> bool {
		!(0..7).contains(&point.x()) || rock_locs.contains(&point)
	}

	/// Use BFS to find all reachable points and remove any we can't reach from the
	/// starting point, as these points can't possibly affect a falling rock (they are
	/// literally unreachable). We need to do this to keep memory usage manageable
	fn prune(rock_locs: &mut Set<Point>, start: Point) {
		// queue.first() is the back, queue.last() is the front
		let mut queue = vec![start];

		let mut seen = queue.iter().copied().collect::<Set<_>>();
		let mut rocks_in_play = Set::new();

		while let Some(point) = queue.pop() {
			if rock_locs.contains(&point) {
				rocks_in_play.insert(point);
				continue;
			}

			for new_point in get_nsew_adjacent(point.as_row_col_tuple(), ..start.y(), 0..7)
				.into_iter()
				.flatten()
				.map(|(y, x)| Point(x, y))
			{
				if seen.insert(new_point) {
					queue.push(new_point);
				}
			}
		}

		rock_locs.retain(|p| rocks_in_play.contains(p));
	}

	let mut highest_rock = 0;

	let mut rock_locs = (0..7).map(|x| Point(x, 0)).collect::<Set<_>>();

	let mut jet_iter = chamber.jet_iter();
	for (i, rock_kind) in (1_usize..).zip(chamber.rock_iter()).take(n) {
		let mut lower_left = Point(2, highest_rock + 4);
		if i % 10_000_000 == 0 {
			prune(&mut rock_locs, lower_left);
		}

		for jet in jet_iter.by_ref() {
			let tentative_lower_left = lower_left.translated(jet.dx(), 0);
			let tentative_rock = Rock::new(rock_kind, tentative_lower_left);
			let tentative_leading_edge_is_blocked = match jet {
				Jet::Left => tentative_rock
					.left_edge()
					.any(|p| is_blocked(&rock_locs, p)),
				Jet::Right => tentative_rock
					.right_edge()
					.any(|p| is_blocked(&rock_locs, p)),
			};
			if !tentative_leading_edge_is_blocked {
				lower_left = tentative_lower_left;
			}

			let tentative_lower_left = lower_left.translated(0, -1);
			let tentative_rock = Rock::new(rock_kind, tentative_lower_left);
			if tentative_rock
				.bottom_edge()
				.any(|p| is_blocked(&rock_locs, p))
			{
				break;
			}

			lower_left = tentative_lower_left;
		}

		let final_rock = Rock::new(rock_kind, lower_left);
		rock_locs.extend(final_rock.occupied());
		highest_rock = highest_rock.max(lower_left.y() + rock_kind.height() - 1);

		// i += 1;
	}

	highest_rock
}
// end::setup[]

// tag::pt1[]
fn pt1(chamber: &Chamber) -> Ans {
	solve(chamber, 2022)
}
// end::pt1[]

// tag::pt2[]
fn pt2(chamber: &Chamber) -> Ans {
	solve(chamber, 1_000_000)
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
			(pt1, 3068),
			(pt2, 1_514_285_714_288),
		);
		run_tests(
			&read_input(include_str!("input.txt")).unwrap(),
			(pt1, 3147),
			(pt2, 2615),
		);
	}
}
