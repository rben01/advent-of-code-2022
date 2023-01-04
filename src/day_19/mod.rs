// tag::setup[]
use crate::{read_file, regex, Answer, ToResultDefaultErr};
use priority_queue::PriorityQueue;
use std::{
	collections::HashSet,
	ops::{Add, AddAssign, Index, IndexMut, Mul, Sub, SubAssign},
	str::FromStr,
};
use strum::EnumCount;
use strum_macros::EnumCount;

type Num = u32;
type Ans = Num;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let blueprints = read_input(input).expect("could not read input");
	(
		19,
		(
			pt1(blueprints.iter().cloned()),
			pt2(blueprints.iter().cloned()),
		),
	)
		.into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("sample_input.txt"))
}

#[derive(Debug, Clone, Copy, EnumCount)]
#[repr(usize)]
enum ResourceKind {
	Ore,
	Clay,
	Obsidian,
	Geode,
}

impl ResourceKind {
	const ALL: [ResourceKind; ResourceKind::COUNT] = [
		ResourceKind::Ore,
		ResourceKind::Clay,
		ResourceKind::Obsidian,
		ResourceKind::Geode,
	];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ResourceCounts([Num; ResourceKind::COUNT]);

impl ResourceCounts {
	fn empty() -> Self {
		Self(Default::default())
	}

	fn n(n: Num, kind: ResourceKind) -> Self {
		let mut counts = Self([0; ResourceKind::COUNT]);
		counts[kind] = n;
		counts
	}

	fn indexed(self) -> [(ResourceKind, Num); ResourceKind::COUNT] {
		ResourceKind::ALL.zip(self.0)
	}

	fn any_exceeds(self, other: Self) -> bool {
		self.0
			.zip(other.0)
			.into_iter()
			.any(|(mine, theirs)| mine > theirs)
	}
}

impl Index<ResourceKind> for ResourceCounts {
	type Output = Num;

	fn index(&self, index: ResourceKind) -> &Self::Output {
		&self.0[index as usize]
	}
}

impl IndexMut<ResourceKind> for ResourceCounts {
	fn index_mut(&mut self, index: ResourceKind) -> &mut Self::Output {
		&mut self.0[index as usize]
	}
}

impl Add for ResourceCounts {
	type Output = Self;

	fn add(self, rhs: Self) -> Self::Output {
		Self(self.0.zip(rhs.0).map(|(x, y)| x + y))
	}
}

impl AddAssign for ResourceCounts {
	fn add_assign(&mut self, rhs: Self) {
		*self = *self + rhs;
	}
}

impl Sub for ResourceCounts {
	type Output = Self;

	fn sub(self, rhs: Self) -> Self::Output {
		Self(self.0.zip(rhs.0).map(|(x, y)| x - y))
	}
}

impl SubAssign for ResourceCounts {
	fn sub_assign(&mut self, rhs: Self) {
		*self = *self - rhs;
	}
}

impl Mul<Num> for ResourceCounts {
	type Output = Self;

	fn mul(self, rhs: Num) -> Self::Output {
		Self(self.0.map(|n| n * rhs))
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Blueprint {
	blueprint_number: Num,
	ore_rbt_ore_cost: Num,
	clay_rbt_ore_cost: Num,
	obsidian_rbt_ore_cost: Num,
	obsidian_rbt_clay_cost: Num,
	geode_rbt_ore_cost: Num,
	geode_rbt_obsidian_cost: Num,
}

impl FromStr for Blueprint {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let line_re = regex!(concat!(
			r#"Blueprint (?P<blueprint_number>\d+): "#,
			r#"Each ore robot costs (?P<ore_rbt_ore_cost>\d+) ore\. "#,
			r#"Each clay robot costs (?P<clay_rbt_ore_cost>\d+) ore\. "#,
			r#"Each obsidian robot costs (?P<obsidian_rbt_ore_cost>\d+) ore and (?P<obsidian_rbt_clay_cost>\d+) clay\. "#,
			r#"Each geode robot costs (?P<geode_rbt_ore_cost>\d+) ore and (?P<geode_rbt_obsidian_cost>\d+) obsidian\."#,
		));

		let caps = line_re.captures(s).to_result()?;
		let [blueprint_number, ore_rbt_ore_cost, clay_rbt_ore_cost, obsidian_rbt_ore_cost, obsidian_rbt_clay_cost, geode_rbt_ore_cost, geode_rbt_obsidian_cost] =
			[
				"blueprint_number",
				"ore_rbt_ore_cost",
				"clay_rbt_ore_cost",
				"obsidian_rbt_ore_cost",
				"obsidian_rbt_clay_cost",
				"geode_rbt_ore_cost",
				"geode_rbt_obsidian_cost",
			]
			.try_map(|name| {
				caps.name(name)
					.to_result()
					.and_then(|n| n.as_str().parse::<Num>().map_err(|e| e.to_string()))
			})?;

		Ok(Blueprint {
			blueprint_number,
			ore_rbt_ore_cost,
			clay_rbt_ore_cost,
			obsidian_rbt_ore_cost,
			obsidian_rbt_clay_cost,
			geode_rbt_ore_cost,
			geode_rbt_obsidian_cost,
		})
	}
}

impl Blueprint {
	fn cost_of_robot(&self, kind: ResourceKind) -> ResourceCounts {
		use ResourceKind::*;
		let &Self {
			blueprint_number: _,
			ore_rbt_ore_cost,
			clay_rbt_ore_cost,
			obsidian_rbt_ore_cost,
			obsidian_rbt_clay_cost,
			geode_rbt_ore_cost,
			geode_rbt_obsidian_cost,
		} = self;
		let mut counts = ResourceCounts::empty();
		match kind {
			Ore => counts[Ore] = ore_rbt_ore_cost,
			Clay => counts[Ore] = clay_rbt_ore_cost,
			Obsidian => {
				counts[Ore] = obsidian_rbt_ore_cost;
				counts[Clay] = obsidian_rbt_clay_cost;
			}
			Geode => {
				counts[Ore] = geode_rbt_ore_cost;
				counts[Obsidian] = geode_rbt_obsidian_cost;
			}
		};
		counts
	}

	fn net_cost(&self, robots: ResourceCounts) -> ResourceCounts {
		let mut costs = ResourceCounts::empty();
		for (robot_kind, count) in robots.indexed() {
			costs += self.cost_of_robot(robot_kind) * count;
		}
		costs
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Factory {
	blueprint: Blueprint,
	resources: ResourceCounts,
	robots: ResourceCounts,
}

impl Factory {
	fn new(blueprint: Blueprint) -> Self {
		let resources = ResourceCounts::empty();
		let robots = {
			let mut r = ResourceCounts::empty();
			r[ResourceKind::Ore] = 1;
			r
		};

		Self {
			blueprint,
			resources,
			robots,
		}
	}

	/// Perform one full "tick":
	/// - Start building `robots_to_build`
	/// - Use current robots to gather resources
	/// - Finish building `robots_to_build`, adding them to `robots`
	/// ## Panics
	/// If there are not enough resources to build the specified `robots_to_build`
	fn tick(&self, robots_to_build: ResourceCounts) -> Self {
		let Self {
			blueprint,
			mut resources,
			mut robots,
		} = self;

		// let should_print =
		// 	resources == ResourceCounts([3, 7, 1, 0]) && robots == ResourceCounts([1, 3, 1, 0]);

		// if should_print {
		// 	println!("{self:?}, {robots_to_build:?}");
		// }

		// Gather resources
		let addtl_resources = robots;

		// Begin construction of new robots
		let mut robots_under_construction = ResourceCounts::empty();
		for (robot_kind, count) in robots_to_build.indexed() {
			let costs = blueprint.cost_of_robot(robot_kind) * count;
			for (kind, cost) in costs.indexed() {
				resources[kind] = resources[kind].checked_sub(cost).unwrap_or_else(|| {
					panic!("insufficient resources to build {robots_to_build:?} from {self:?}")
				});
			}
			robots_under_construction[robot_kind] += count;
		}

		resources += addtl_resources;
		robots += robots_under_construction;

		// if should_print {
		// 	let x = Self {
		// 		blueprint: self.blueprint.clone(),
		// 		resources,
		// 		robots,
		// 	};
		// 	// println!("{x:?}");
		// }

		Self {
			blueprint: blueprint.clone(),
			resources,
			robots,
		}
	}

	fn get_all_builds(&self) -> Vec<ResourceCounts> {
		let Self {
			blueprint,
			resources,
			..
		} = self;
		let resources = *resources;

		let mut seen = HashSet::new();
		let mut builds = vec![ResourceCounts::empty()];

		let mut queue = vec![ResourceCounts::empty()];
		while let Some(robot_counts) = queue.pop() {
			for kind in ResourceKind::ALL {
				let new_counts = robot_counts + ResourceCounts::n(1, kind);
				let new_cost = blueprint.net_cost(new_counts);
				if new_cost.any_exceeds(resources) || !seen.insert(new_counts) {
					continue;
				}

				queue.push(new_counts);

				// If adding another of this robot would make the build no longer
				// affordable, then this build is "final" (worthy of being returned)
				if (new_cost + blueprint.cost_of_robot(kind)).any_exceeds(resources) {
					builds.push(new_counts);
				}
			}
		}

		builds
	}

	/// An upper bound of geodes we could conceivably manufacture with `n_ticks` time
	/// left: robots don't cost any resources! We can build as many robots of each kind
	/// per turn as we'd normally be able to, ignoring what we spend building robots of
	/// other kinds, and at the end of the turn we don't deduct the resource spend.
	/// ## Returns
	/// (# geodes, # obsidian, # clay, # ore). We only really care about # geodes but we
	/// use the rest to break ties
	fn priority(&self, n_ticks: usize) -> (Num, Num, Num, Num) {
		let Self {
			blueprint,
			resources,
			robots,
			..
		} = self;

		let mut resources = *resources;
		let mut robots = *robots;

		for _ in 0..n_ticks {
			let mut addtl_robots = ResourceCounts::empty();
			for robot_kind in ResourceKind::ALL.into_iter().rev() {
				let cost = blueprint.cost_of_robot(robot_kind);
				let n_buildable = ResourceKind::ALL
					.into_iter()
					.filter_map(|r| resources[r].checked_div(cost[r]))
					.min()
					.unwrap(); // we know that the costs are not all 0
				addtl_robots[robot_kind] = n_buildable;
				// if !matches!(robot_kind, ResourceKind::Ore) {
				// 	resources -= cost * n_buildable;
				// }
			}

			// Subtract what we spent on additional robots this turn
			// resources = ResourceCounts(
			// 	resources
			// 		.0
			// 		.zip(addtl_robots_cost.0)
			// 		.map(|(res_count, cost)| res_count.checked_sub(cost).unwrap_or_default()),
			// );
			// Add what was produced by robots
			resources += robots;

			// Produce new robots
			robots += addtl_robots;
		}

		resources.0.reverse();
		let [g, b, c, o] = resources.0;
		(g, b, c, o)
	}
}

fn solve(blueprint: Blueprint, n_ticks: usize) -> Factory {
	let factory = Factory::new(blueprint);
	let mut pq = PriorityQueue::new();
	{
		let priority0 = factory.priority(n_ticks);
		pq.push((n_ticks, factory), priority0);
	}

	let mut seen = HashSet::new();

	let mut i = 1;
	while let Some(((n_ticks, factory), _)) = pq.pop() {
		if n_ticks == 0 {
			return factory;
		}

		if i % 10000 == 0 {
			println!(
				"{n_ticks:?}, {:?}, {:?}, {:?}, {:?}",
				pq.peek(),
				seen.iter().next(),
				pq.len(),
				seen.len()
			);
		}
		i += 1;

		let builds = factory.get_all_builds();
		for robots_to_build in builds {
			let factory = factory.tick(robots_to_build);
			if !seen.insert(factory.clone()) {
				continue;
			}

			let priority = factory.priority(n_ticks - 1);
			assert!(pq.push((n_ticks - 1, factory), priority).is_none());
		}
	}

	unreachable!("unexpectedly did not find n_ticks = 0")
}

fn read_input(input: &str) -> Result<Vec<Blueprint>, String> {
	input.lines().map(|line| line.parse()).collect()
}
// end::setup[]

// tag::pt1[]
fn pt1(blueprints: impl IntoIterator<Item = Blueprint>) -> Ans {
	let blueprints = blueprints.into_iter().collect::<Vec<_>>();
	let value = blueprints
		.into_iter()
		.map(|bp| {
			let num = bp.blueprint_number;
			let factory = solve(bp, 24);
			println!("{num:?}");
			dbg!(factory.resources[ResourceKind::Geode] * num)
		})
		.sum();
	// let f = Factory {
	// 	blueprint: Blueprint {
	// 		blueprint_number: 1,
	// 		ore_rbt_ore_cost: 4,
	// 		clay_rbt_ore_cost: 2,
	// 		obsidian_rbt_ore_cost: 3,
	// 		obsidian_rbt_clay_cost: 14,
	// 		geode_rbt_ore_cost: 2,
	// 		geode_rbt_obsidian_cost: 7,
	// 	},
	// 	resources: ResourceCounts([2, 4, 0, 0]),
	// 	robots: ResourceCounts([1, 3, 1, 0]),
	// };
	// println!("{:?}", f.priority(12));
	// println!("{:?}", f.get_all_builds());
	// println!("{:?}", f.tick(ResourceCounts([0, 0, 0, 0])).priority(11));
	// println!("{:?}", f.tick(ResourceCounts([0, 1, 0, 0])).priority(11));
	// println!("{:?}", f.priority(5));
	// println!("{:?}", f.get_all_builds());
	// println!(
	// 	"{:?}",
	// 	f.tick(ResourceCounts([0, 0, 0, 1])) // .tick(ResourceCounts::empty())
	// 	                                     // .tick(ResourceCounts::empty())
	// 	                                     // .tick(ResourceCounts::empty())
	// 	                                     // .tick(ResourceCounts::empty())
	// );
	// println!("{:?}", solve(blueprints.swap_remove(0), 24));

	// println!("{:?}", scenarios);
	// println!("{:?}, {:?}", scenarios[0], scenarios[0].robot_ore_costs());
	// let bp = Blueprint {
	// 	blueprint_number: 1,
	// 	ore_rbt_ore_cost: 2,
	// 	clay_rbt_ore_cost: 3,
	// 	obsidian_rbt_ore_cost: 4,
	// 	obsidian_rbt_clay_cost: 5,
	// 	geode_rbt_ore_cost: 6,
	// 	geode_rbt_obsidian_cost: 7,
	// };
	// println!("{bp:?}");
	// let f = Factory {
	// 	blueprint: bp,
	// 	resources: ResourceCounts([100, 10, 100, 100]),
	// 	robots: ResourceCounts([0, 0, 0, 1]),
	// 	robots_under_construction: ResourceCounts::empty(),
	// };
	// println!("{:?}", f.get_all_builds().len());
	// println!("{:?}", f.priority(5));
	value
}
// end::pt1[]

// tag::pt2[]
fn pt2(cubes: impl IntoIterator<Item = Blueprint>) -> Ans {
	0
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
			read_input(&read_file!("sample_input.txt"))
				.unwrap()
				.iter()
				.cloned(),
			(pt1, 3068),
			(pt2, 0),
		);
		run_tests(
			read_input(&read_file!("input.txt"))
				.unwrap()
				.iter()
				.cloned(),
			(pt1, 3147),
			(pt2, 0),
		);
	}
}
