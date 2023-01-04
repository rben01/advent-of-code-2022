// tag::setup[]
use crate::{read_file, utils::get_xyz_adjacent, Answer, ToResultDefaultErr};
use itertools::iproduct;
use std::{collections::HashSet, ops::RangeBounds, str::FromStr};

type Set<T> = HashSet<T>;
type Ans = usize;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let cubes = read_input(input).expect("could not read input");
	(18, (pt1(cubes.iter().copied()), pt2(cubes.iter().copied()))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("input.txt"))
}

type Num = i32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Cube(Num, Num, Num);

impl Cube {
	fn x(self) -> Num {
		self.0
	}
	fn y(self) -> Num {
		self.1
	}
	fn z(self) -> Num {
		self.2
	}
}

impl From<(Num, Num, Num)> for Cube {
	fn from((x, y, z): (Num, Num, Num)) -> Self {
		Self(x, y, z)
	}
}

impl From<Cube> for (Num, Num, Num) {
	fn from(Cube(x, y, z): Cube) -> Self {
		(x, y, z)
	}
}

impl FromStr for Cube {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let mut comps = s.split(',');
		let [x, y, z] = [(); 3].try_map(|_| {
			comps
				.next()
				.to_result()?
				.parse()
				.map_err(|e| format!("{e:?}"))
		})?;

		if comps.count() != 0 {
			return Err(format!("got more than 3 components in {s:?}"));
		}

		Ok(Self(x, y, z))
	}
}

fn read_input(input: &str) -> Result<Vec<Cube>, String> {
	input.lines().map(|line| line.parse()).collect()
}
// end::setup[]

// tag::pt1[]
fn pt1(cubes: impl IntoIterator<Item = Cube>) -> Ans {
	let cubes = cubes.into_iter().collect::<Set<_>>();

	cubes
		.iter()
		.flat_map(|&cube| get_xyz_adjacent(cube, .., .., ..))
		.flatten()
		.filter(|&coords| !cubes.contains(&coords))
		.count()
}
// end::pt1[]

// tag::pt2[]
fn pt2(cubes: impl IntoIterator<Item = Cube>) -> Ans {
	fn get_range_bounds(
		iter: impl IntoIterator<Item = Cube>,
		coord_getter: impl Fn(Cube) -> Num,
	) -> (Num, Num) {
		let (min, max) =
			iter.into_iter()
				.fold((Num::MAX, Num::MIN), |(curr_min, curr_max), cube| {
					let coord = coord_getter(cube);
					let min = curr_min.min(coord);
					let max = curr_max.max(coord);
					(min, max)
				});
		(min - 1, max + 1)
	}

	/// Search for a path from `start` to any of `end_points`, avoiding `cubes`. Returns
	/// a set of new points to add to `end_points`, or None if no path was found (`start`
	/// is interior).
	fn find_path(
		cubes: &Set<Cube>,
		start: Cube,
		end_points: &Set<Cube>,
		x_range: impl RangeBounds<Num>,
		y_range: impl RangeBounds<Num>,
		z_range: impl RangeBounds<Num>,
	) -> Option<Set<Cube>> {
		let mut seen = Set::new();
		let mut queue = vec![start];

		while let Some(point) = queue.pop() {
			for adj in get_xyz_adjacent(point, .., .., ..).into_iter().flatten() {
				if end_points.contains(&adj) {
					return Some(seen);
				}

				if cubes.contains(&adj)
					|| !seen.insert(adj) || !(x_range.contains(&adj.x())
					&& y_range.contains(&adj.y())
					&& z_range.contains(&adj.z()))
				{
					continue;
				}

				queue.push(adj);
			}
		}

		None
	}

	let cubes = cubes.into_iter().collect::<Set<_>>();

	// The initial exterior points: the eight corners bounding the cube
	let (x_min, x_max) = get_range_bounds(cubes.iter().copied(), Cube::x);
	let (y_min, y_max) = get_range_bounds(cubes.iter().copied(), Cube::y);
	let (z_min, z_max) = get_range_bounds(cubes.iter().copied(), Cube::z);

	let mut exterior_points = iproduct!([x_min, x_max], [y_min, y_max], [z_min, z_max])
		.map(Cube::from)
		.collect();

	let mut n_exposed_faces = 0;

	for &cube in &cubes {
		for face in get_xyz_adjacent(cube, .., .., ..).into_iter().flatten() {
			if cubes.contains(&face) {
				continue;
			}

			if let Some(new_exterior_points) = find_path(
				&cubes,
				face,
				&exterior_points,
				x_min..=x_max,
				y_min..=y_max,
				z_min..=z_max,
			) {
				exterior_points.extend(new_exterior_points);
				n_exposed_faces += 1;
			}
		}
	}

	n_exposed_faces
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
				.copied(),
			(pt1, 3068),
			(pt2, 0),
		);
		run_tests(
			read_input(&read_file!("input.txt"))
				.unwrap()
				.iter()
				.copied(),
			(pt1, 3147),
			(pt2, 0),
		);
	}
}
