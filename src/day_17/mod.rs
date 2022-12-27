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
	ans_for_input(include_str!("sample_input.txt"))
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
enum RockKind {
	FourHorizontal,
	FivePlusSign,
	FiveBackwardsL,
	FourVertical,
	FourSquare,
}

impl RockKind {
	const N_KINDS: usize = 5;

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
	rock_seq: [RockKind; RockKind::N_KINDS],
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

	fn rock_iter(&self, cycle: bool) -> RockIter {
		RockIter {
			chamber: self,
			index: 0,
			cycle,
		}
	}

	fn jet_iter(&self, cycle: bool) -> JetIter {
		JetIter {
			chamber: self,
			index: 0,
			cycle,
		}
	}
}

struct RockIter<'a> {
	chamber: &'a Chamber,
	index: usize,
	cycle: bool,
}

impl Iterator for RockIter<'_> {
	type Item = RockKind;
	fn next(&mut self) -> Option<Self::Item> {
		let Self {
			chamber: Chamber { rock_seq, .. },
			index,
			cycle,
		} = self;

		let rock = *rock_seq.get(*index)?;

		*index += 1;
		if *cycle {
			*index %= rock_seq.len();
		}

		Some(rock)
	}
}

struct JetIter<'a> {
	chamber: &'a Chamber,
	index: usize,
	cycle: bool,
}

impl Iterator for JetIter<'_> {
	type Item = Jet;
	fn next(&mut self) -> Option<Self::Item> {
		let Self {
			chamber: Chamber { jet_seq, .. },
			index,
			cycle,
		} = self;

		let jet = *jet_seq.get(*index)?;

		*index += 1;
		if *cycle {
			*index %= jet_seq.len();
		}

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
	#[derive(Debug)]
	enum DroppedRockState {
		CameToRest,
		PausedMidFall,
	}

	#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
	struct FrozenFormation {
		rock_mid_fall: Option<DroppedRock>,
		rock_locs: Vec<Point>,
	}

	#[derive(Debug)]
	struct SeenFormationInfo {
		count: usize,
		first_index: usize,
	}

	#[derive(Debug, Default, Clone, Copy)]
	struct RoundInfo {
		n_rocks_dropped: usize,
		n_jets_fired: usize,
		addtl_height: Y,
	}

	impl Add for RoundInfo {
		type Output = Self;
		fn add(self, rhs: Self) -> Self::Output {
			Self {
				n_rocks_dropped: self.n_rocks_dropped + rhs.n_rocks_dropped,
				n_jets_fired: self.n_jets_fired + rhs.n_jets_fired,
				addtl_height: self.addtl_height + rhs.addtl_height,
			}
		}
	}

	impl Sub for RoundInfo {
		type Output = Self;
		fn sub(self, rhs: Self) -> Self::Output {
			Self {
				n_rocks_dropped: self.n_rocks_dropped - rhs.n_rocks_dropped,
				n_jets_fired: self.n_jets_fired - rhs.n_jets_fired,
				addtl_height: self.addtl_height - rhs.addtl_height,
			}
		}
	}

	impl Mul<usize> for RoundInfo {
		type Output = Self;
		fn mul(self, rhs: usize) -> Self::Output {
			Self {
				n_rocks_dropped: self.n_rocks_dropped * rhs,
				n_jets_fired: self.n_jets_fired * rhs,
				addtl_height: self.addtl_height * Y::try_from(rhs).unwrap(),
			}
		}
	}

	#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
	struct DroppedRock {
		rock_kind: RockKind,
		lower_left: Point,
	}

	#[derive(Debug)]
	struct DroppedRockInfo {
		dropped_rock: DroppedRock,
		n_jets_fired: usize,
		did_finish_round: bool,
	}

	fn is_blocked(rock_locs: &BTreeSet<Point>, point: Point) -> bool {
		!(0..7).contains(&point.x()) || rock_locs.contains(&point)
	}

	/// Use BFS to find all reachable points and remove any we can't reach from the
	/// starting point, as these points can't possibly affect a falling rock (they are
	/// literally unreachable). We need to do this to keep memory usage manageable. Also
	/// we subtract `y_max` from all of them so that if multiple configurations have top
	/// parts with the same shape (but perhaps at different heights) we can still
	/// identify them as "the same". And also so that every starting formation's highest
	/// point is 0, which means additional height = final height
	fn prune(
		rock_locs: &BTreeSet<Point>,
		rock_mid_fall: &mut Option<DroppedRock>,
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
		if let Some(rock_mid_fall) = rock_mid_fall.as_mut() {
			rock_mid_fall.lower_left = rock_mid_fall.lower_left.translated(0, -y_max);
		}
		rock_locs
			.iter()
			.filter_map(|p| rocks_in_play.contains(p).then_some(p.translated(0, -y_max)))
			.collect()
	}

	/// Given an iterator over the jets, drop a single rock
	fn drop_rock<Jets>(
		formation: &BTreeSet<Point>,
		rock_kind: RockKind,
		p0: Point,
		jet_enumerator: &mut Jets,
		did_finish_round_pred: impl Fn(usize) -> bool,
	) -> (DroppedRockState, DroppedRockInfo)
	where
		Jets: Iterator<Item = (usize, Jet)>,
	{
		let mut lower_left = p0;

		for (n_jets_fired, (jet_idx, jet)) in (1..).zip(jet_enumerator) {
			let tentative_lower_left = lower_left.translated(jet.dx(), 0);
			let tentative_rock = Rock::new(rock_kind, tentative_lower_left);
			let tentative_leading_edge_is_blocked = match jet {
				Jet::Left => tentative_rock.left_edge().any(|p| is_blocked(formation, p)),
				Jet::Right => tentative_rock
					.right_edge()
					.any(|p| is_blocked(formation, p)),
			};
			if !tentative_leading_edge_is_blocked {
				lower_left = tentative_lower_left;
			}
			let did_finish_round = did_finish_round_pred(jet_idx);
			dbg!(jet_idx, did_finish_round);

			let tentative_lower_left = lower_left.translated(0, -1);
			let tentative_rock = Rock::new(rock_kind, tentative_lower_left);
			if tentative_rock
				.bottom_edge()
				.any(|p| is_blocked(formation, p))
			{
				return (
					DroppedRockState::CameToRest,
					DroppedRockInfo {
						dropped_rock: DroppedRock {
							rock_kind,
							lower_left,
						},
						n_jets_fired,
						did_finish_round,
					},
				);
			}
			lower_left = tentative_lower_left;

			if did_finish_round {
				return (
					DroppedRockState::PausedMidFall,
					DroppedRockInfo {
						dropped_rock: DroppedRock {
							rock_kind,
							lower_left,
						},
						n_jets_fired,
						did_finish_round,
					},
				);
			}
		}

		unreachable!("unexpectedly exhausted the jet iterator")
	}

	#[must_use]
	fn record_round(
		orig_formation: Vec<Point>,
		rock_mid_fall: Option<DroppedRock>,
		round_index: &mut usize,
		round_stats: RoundInfo,
		seen_formations: &mut BTreeMap<FrozenFormation, SeenFormationInfo>,
		rounds_cumm_info: &mut Vec<RoundInfo>,
	) -> Option<usize> {
		let seen_formation_info = seen_formations
			.entry(FrozenFormation {
				rock_mid_fall,
				rock_locs: orig_formation,
			})
			.or_insert(SeenFormationInfo {
				count: 0,
				first_index: *round_index,
			});

		seen_formation_info.count += 1;

		let prev_round_cumm_stats = rounds_cumm_info.last().copied().unwrap_or_default();
		let this_round_cumm_stats = RoundInfo {
			n_rocks_dropped: prev_round_cumm_stats.n_rocks_dropped + round_stats.n_rocks_dropped,
			n_jets_fired: prev_round_cumm_stats.n_jets_fired + round_stats.n_jets_fired,
			addtl_height: prev_round_cumm_stats.addtl_height + round_stats.addtl_height,
		};
		rounds_cumm_info.push(this_round_cumm_stats);
		*round_index += 1;

		if seen_formation_info.count >= 2 {
			Some(seen_formation_info.first_index)
		} else {
			None
		}
	}

	fn get_final_height(
		n_rocks_max: usize,
		net_rocks_dropped: usize,
		rounds_cumm_info: &[RoundInfo],
		cycle_first_index: usize,
	) -> Y {
		let start_info = rounds_cumm_info[cycle_first_index];
		let end_info = rounds_cumm_info[rounds_cumm_info.len() - 1];
		let per_cycle_info = end_info - start_info;

		let cycle_len = rounds_cumm_info.len() - cycle_first_index;
		let n_complete_cycles_remaining =
			(n_rocks_max - net_rocks_dropped) / (per_cycle_info.n_rocks_dropped);

		println!("{rounds_cumm_info:?}");
		println!("{n_rocks_max}, {cycle_first_index}, {net_rocks_dropped}, {cycle_len:?}",);
		println!(
			"{per_cycle_info:?}, {n_complete_cycles_remaining:?}, {:?}",
			start_info + per_cycle_info * n_complete_cycles_remaining
		);

		// println!(
		// 	"{first_index:?}, {rounds_info:?}, {:?}, {cycle_len:?}, {}",
		// 	rounds_info[*first_index],
		// 	rounds_info.len()
		// );
		// println!("{formation_key:?}");

		// let n_addtl_runs = (n - net_rocks_dropped) / n_rocks_dropped;
		// println!("{n}, {net_rocks_dropped}, {n_rocks_dropped}, {n_addtl_runs}, {addtl_height}");
		// net_height += addtl_height * (n_addtl_runs as i64);

		0
	}

	let mut seen_formations = BTreeMap::new();
	let mut rounds_cumm_info = Vec::<RoundInfo>::new();
	let mut this_formation = (0..7).map(|x| Point(x, 0)).collect::<BTreeSet<_>>();

	let mut net_height = 0;
	let mut net_rocks_dropped = 0;

	let mut jet_iter = chamber.jet_iter(true);
	let mut rock_mid_fall: Option<DroppedRock> = None;

	let mut round_index = 0;
	while net_rocks_dropped < n {
		let orig_rock_mid_fall = rock_mid_fall;
		let orig_formation = this_formation.iter().copied().collect::<Vec<_>>();

		let mut jet_enumerator = jet_iter.by_ref().enumerate();

		let mut this_run_y_max = 0;
		let mut this_run_n_rocks_dropped = 0;
		let mut this_run_n_jets_fired = 0;

		if let Some(DroppedRock {
			rock_kind,
			lower_left,
		}) = rock_mid_fall
		{
			dbg!(rock_mid_fall);
			let (
				dr_state,
				DroppedRockInfo {
					dropped_rock,
					n_jets_fired,
					did_finish_round,
				},
			) = drop_rock(
				&this_formation,
				rock_kind,
				lower_left,
				jet_enumerator.by_ref(),
				|jet_idx| dbg!((jet_idx + 1) % chamber.jet_seq.len()) == 0,
			);

			this_run_n_jets_fired += n_jets_fired;

			match dr_state {
				DroppedRockState::CameToRest => {
					let settled_rock = Rock::new(rock_kind, lower_left);
					this_formation.extend(settled_rock.occupied());

					this_run_y_max = this_run_y_max.max(lower_left.y() + rock_kind.height() - 1);
					rock_mid_fall = None;
				}
				DroppedRockState::PausedMidFall => {
					rock_mid_fall = Some(dropped_rock);
				}
			}

			if did_finish_round {
				if let Some(cycle_first_index) = record_round(
					orig_formation,
					orig_rock_mid_fall,
					&mut round_index,
					RoundInfo {
						n_rocks_dropped: this_run_n_rocks_dropped,
						n_jets_fired: this_run_n_jets_fired,
						addtl_height: this_run_y_max,
					},
					&mut seen_formations,
					&mut rounds_cumm_info,
				) {
					return get_final_height(
						n,
						net_rocks_dropped,
						&rounds_cumm_info,
						cycle_first_index,
					);
				};
				continue;
			}
		}

		'rocks: for (rock_idx, rock_kind) in chamber.rock_iter(true).enumerate() {
			dbg!(rock_idx);

			let (
				dr_state,
				DroppedRockInfo {
					dropped_rock,
					n_jets_fired,
					did_finish_round,
				},
			) = drop_rock(
				&this_formation,
				rock_kind,
				Point(2, this_run_y_max + 4),
				jet_enumerator.by_ref(),
				|jet_idx| {
					dbg!((rock_idx + 1) % chamber.rock_seq.len()) == 0
						&& dbg!((jet_idx + 1) % chamber.jet_seq.len()) == 0
				},
			);

			this_run_n_jets_fired += n_jets_fired;

			match dr_state {
				DroppedRockState::CameToRest => {
					let settled_rock = Rock::new(rock_kind, dropped_rock.lower_left);
					this_formation.extend(settled_rock.occupied());

					rock_mid_fall = None;
				}
				DroppedRockState::PausedMidFall => {
					rock_mid_fall = Some(dropped_rock);
				}
			}

			this_run_y_max = this_run_y_max
				.max(dropped_rock.lower_left.y() + dropped_rock.rock_kind.height() - 1);

			this_run_n_rocks_dropped += 1;
			net_rocks_dropped += 1;

			if net_rocks_dropped >= n || did_finish_round {
				break 'rocks;
			}
		}
		// println!("nrd: {net_rocks_dropped}");

		this_formation = prune(
			&this_formation,
			&mut rock_mid_fall,
			Point(2, this_run_y_max + 1),
		);

		if let Some(cycle_first_index) = record_round(
			orig_formation,
			orig_rock_mid_fall,
			&mut round_index,
			RoundInfo {
				n_rocks_dropped: this_run_n_rocks_dropped,
				n_jets_fired: this_run_n_jets_fired,
				addtl_height: this_run_y_max,
			},
			&mut seen_formations,
			&mut rounds_cumm_info,
		) {
			return get_final_height(n, net_rocks_dropped, &rounds_cumm_info, cycle_first_index);
		};

		net_height += this_run_y_max;
		println!("{:?}", seen_formations.len());
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
			(pt2, 2615),
		);
	}
}
