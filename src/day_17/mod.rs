// tag::setup[]
use crate::{utils::get_nsew_adjacent, Answer};
use std::{
	collections::{BTreeMap, BTreeSet},
	ops::{Add, Mul, Sub},
};

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let chamber = read_input(input).expect("could not read input");
	(17, (pt1(&chamber), pt2(&chamber))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(include_str!("input.txt"))
}

type Ans = i64;
type X = i8;
type Y = i64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum RockKind {
	FourHorizontal,
	FivePlusSign,
	FiveBackwardsL,
	FourVertical,
	FourSquare,
}

impl From<usize> for RockKind {
	fn from(value: usize) -> Self {
		use RockKind::*;
		match value % 5 {
			0 => FourHorizontal,
			1 => FivePlusSign,
			2 => FiveBackwardsL,
			3 => FourVertical,
			4 => FourSquare,
			_ => unreachable!("i % 5 must be in 0..=4"),
		}
	}
}

impl RockKind {
	const FIRST: Self = Self::FourHorizontal;

	fn kind_after(self) -> Self {
		Self::from((self as usize) + 1)
	}

	fn occupied_rel(self) -> &'static [Point] {
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

	fn left_edge_rel(self) -> &'static [Point] {
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

	fn right_edge_rel(self) -> &'static [Point] {
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

	fn bottom_edge_rel(self) -> &'static [Point] {
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

	fn height(self) -> Y {
		use RockKind::*;
		match self {
			FourHorizontal => 1,
			FourSquare => 2,
			FivePlusSign | FiveBackwardsL => 3,
			FourVertical => 4,
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
	jet_seq: Vec<Jet>,
}

impl Chamber {
	fn new(gas_jet_seq: Vec<Jet>) -> Self {
		Self {
			jet_seq: gas_jet_seq,
		}
	}

	fn jet_iter(&self) -> JetIter {
		JetIter {
			chamber: self,
			index: 0,
		}
	}
}

#[derive(Clone)]
struct JetIter<'a> {
	chamber: &'a Chamber,
	index: usize,
}

impl Iterator for JetIter<'_> {
	type Item = Jet;
	fn next(&mut self) -> Option<Self::Item> {
		let Self {
			chamber: Chamber { jet_seq, .. },
			index,
		} = self;

		let jet = *jet_seq.get(*index)?;
		*index += 1;
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
	#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
	struct RoundStartInfo {
		rock_kind: RockKind,
		falling_rock_lower_left: Option<Point>,
		rock_locs: Vec<Point>,
	}

	#[derive(Debug, Default, Clone, Copy)]
	struct RoundEndStats {
		n_rocks_dropped: usize,
		n_jets_fired: usize,
		height_gained: Y,
	}

	#[derive(Debug)]
	struct SeenFormationInfo {
		count: usize,
		first_index: usize,
	}

	impl Add for RoundEndStats {
		type Output = Self;
		fn add(self, rhs: Self) -> Self::Output {
			Self {
				n_rocks_dropped: self.n_rocks_dropped + rhs.n_rocks_dropped,
				n_jets_fired: self.n_jets_fired + rhs.n_jets_fired,
				height_gained: self.height_gained + rhs.height_gained,
			}
		}
	}

	impl Sub for RoundEndStats {
		type Output = Self;
		fn sub(self, rhs: Self) -> Self::Output {
			Self {
				n_rocks_dropped: self.n_rocks_dropped - rhs.n_rocks_dropped,
				n_jets_fired: self.n_jets_fired - rhs.n_jets_fired,
				height_gained: self.height_gained - rhs.height_gained,
			}
		}
	}

	impl Mul<usize> for RoundEndStats {
		type Output = Self;
		fn mul(self, rhs: usize) -> Self::Output {
			Self {
				n_rocks_dropped: self.n_rocks_dropped * rhs,
				n_jets_fired: self.n_jets_fired * rhs,
				height_gained: self.height_gained * Y::try_from(rhs).unwrap(),
			}
		}
	}

	enum RockFinishedFalling {
		Yes,
		No,
	}

	struct StepInfo {
		rock_kind: RockKind,
		rock_locs: BTreeSet<Point>,
		lower_left: Point,
		this_run_y_max: Y,
	}

	fn is_blocked(rock_locs: &BTreeSet<Point>, point: Point) -> bool {
		!(0..7).contains(&point.x()) || rock_locs.contains(&point)
	}

	fn fire_jet(jet: Jet, step_info: StepInfo) -> (RockFinishedFalling, StepInfo) {
		let StepInfo {
			mut rock_kind,
			mut rock_locs,
			mut lower_left,
			mut this_run_y_max,
		} = step_info;

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
			// Rock came to rest
			let settled_rock = Rock::new(rock_kind, lower_left);
			rock_locs.extend(settled_rock.occupied());
			this_run_y_max = this_run_y_max.max(lower_left.y() + rock_kind.height() - 1);

			lower_left = Point(2, this_run_y_max + 4);

			rock_kind = rock_kind.kind_after();

			return (
				RockFinishedFalling::Yes,
				StepInfo {
					rock_kind,
					rock_locs,
					lower_left,
					this_run_y_max,
				},
			);
		}

		// Rock still falling
		lower_left = tentative_lower_left;

		(
			RockFinishedFalling::No,
			StepInfo {
				rock_kind,
				rock_locs,
				lower_left,
				this_run_y_max,
			},
		)
	}

	/// Use BFS to find all reachable points and remove any we can't reach from the
	/// starting point, as these points can't possibly affect a falling rock (they are
	/// literally unreachable). We need to do this to keep memory usage manageable. Also
	/// we subtract `y_max` from all of them so that if multiple configurations have top
	/// parts with the same shape (but perhaps at different heights) we can still
	/// identify them as "the same". And also so that every starting formation's highest
	/// point is 0, which means additional height = final height
	#[must_use]
	fn prune(
		rock_locs: &BTreeSet<Point>,
		rock_mid_fall_lower_left: Option<&mut Point>,
		start: Point,
	) -> BTreeSet<Point> {
		// queue.first() is the back, queue.last() is the front
		let mut queue = vec![start];

		let mut seen = queue.iter().copied().collect::<BTreeSet<_>>();
		let mut rocks_in_play = BTreeSet::new();

		while let Some(point) = queue.pop() {
			if rock_locs.contains(&point) {
				rocks_in_play.insert(point);
				continue;
			}

			for new_point in get_nsew_adjacent((point.x(), point.y()), 0..7, ..=start.y())
				.into_iter()
				.flatten()
				.map(|(x, y)| Point(x, y))
			{
				if seen.insert(new_point) {
					queue.push(new_point);
				}
			}
		}

		let y_max = rock_locs.iter().map(|p| p.y()).max().unwrap();
		if let Some(rock_mid_fall_lower_left) = rock_mid_fall_lower_left {
			*rock_mid_fall_lower_left = rock_mid_fall_lower_left.translated(0, -y_max);
		}
		rock_locs
			.iter()
			.filter_map(|p| rocks_in_play.contains(p).then_some(p.translated(0, -y_max)))
			.collect()
	}

	#[must_use]
	fn record_round(
		orig_state: RoundStartInfo,
		round_index: usize,
		round_stats: RoundEndStats,
		seen_formations: &mut BTreeMap<RoundStartInfo, SeenFormationInfo>,
		rounds_cumm_info: &mut Vec<RoundEndStats>,
	) -> Option<usize> {
		let seen_formation_info = seen_formations
			.entry(orig_state)
			.or_insert(SeenFormationInfo {
				count: 0,
				first_index: round_index,
			});

		seen_formation_info.count += 1;

		let prev_round_cumm_stats = rounds_cumm_info.last().copied().unwrap_or_default();
		let this_round_cumm_stats = prev_round_cumm_stats + round_stats;

		rounds_cumm_info.push(this_round_cumm_stats);

		if seen_formation_info.count >= 2 {
			Some(seen_formation_info.first_index)
		} else {
			None
		}
	}

	let mut seen_formations = BTreeMap::new();
	let mut rounds_cumm_info = Vec::new();
	let mut rock_locs = (0..7).map(|x| Point(x, 0)).collect::<BTreeSet<_>>();

	let mut net_height = 0;
	let mut net_rocks_dropped = 0;

	let mut rock_kind = RockKind::FIRST;
	let mut falling_rock_lower_left = None;

	for round_index in 0.. {
		if net_rocks_dropped >= n {
			break;
		}

		let mut this_run_y_max = 0; // enforced by `prune`
		let mut this_run_n_rocks_dropped = 0;
		let mut this_run_n_jets_fired = 0;

		let orig_state = RoundStartInfo {
			rock_kind,
			falling_rock_lower_left,
			rock_locs: rock_locs.iter().copied().collect(),
		};

		let mut lower_left = falling_rock_lower_left.unwrap_or(Point(2, this_run_y_max + 4));

		for jet in chamber.jet_iter() {
			this_run_n_jets_fired += 1;

			let rock_finished_falling;
			(
				rock_finished_falling,
				StepInfo {
					rock_kind,
					rock_locs,
					lower_left,
					this_run_y_max,
				},
			) = fire_jet(
				jet,
				StepInfo {
					rock_kind,
					rock_locs,
					lower_left,
					this_run_y_max,
				},
			);

			if matches!(rock_finished_falling, RockFinishedFalling::Yes) {
				this_run_n_rocks_dropped += 1;
				net_rocks_dropped += 1;
				if net_rocks_dropped >= n {
					break;
				}
			}
		}

		rock_locs = prune(
			&rock_locs,
			falling_rock_lower_left.as_mut(),
			Point(2, this_run_y_max + 1),
		);

		net_height += this_run_y_max;

		if let Some(cycle_first_index) = record_round(
			orig_state,
			round_index,
			RoundEndStats {
				n_rocks_dropped: this_run_n_rocks_dropped,
				n_jets_fired: this_run_n_jets_fired,
				height_gained: this_run_y_max,
			},
			&mut seen_formations,
			&mut rounds_cumm_info,
		) {
			let pre_cycle_info = rounds_cumm_info[cycle_first_index];
			let post_cycle_info = rounds_cumm_info[rounds_cumm_info.len() - 1];
			let per_cycle_info = post_cycle_info - pre_cycle_info;

			let n_complete_cycles_remaining =
				(n - net_rocks_dropped) / (per_cycle_info.n_rocks_dropped);
			let after_cycles_info = pre_cycle_info + per_cycle_info * n_complete_cycles_remaining;
			net_rocks_dropped = after_cycles_info.n_rocks_dropped;

			let mut this_run_y_max = 0;
			let mut lower_left = falling_rock_lower_left.unwrap_or(Point(2, this_run_y_max + 4));

			for jet in chamber.jet_iter().cycle() {
				let rock_finished_falling;
				(
					rock_finished_falling,
					StepInfo {
						rock_kind,
						rock_locs,
						lower_left,
						this_run_y_max,
					},
				) = fire_jet(
					jet,
					StepInfo {
						rock_kind,
						rock_locs,
						lower_left,
						this_run_y_max,
					},
				);

				if matches!(rock_finished_falling, RockFinishedFalling::Yes) {
					net_rocks_dropped += 1;
					if net_rocks_dropped >= n {
						return after_cycles_info.height_gained + this_run_y_max;
					}
				}
			}
		}
	}

	net_height
}
// end::setup[]

// tag::pt1[]
fn pt1(chamber: &Chamber) -> Ans {
	solve(chamber, 2022)
}
// end::pt1[]

// tag::pt2[]
fn pt2(chamber: &Chamber) -> Ans {
	solve(chamber, 1_000_000_000_000)
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
			(pt2, 1_532_163_742_758),
		);
	}
}
