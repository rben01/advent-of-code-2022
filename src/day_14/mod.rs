// tag::setup[]
use crate::Answer;
use std::collections::BTreeMap;

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let rock = read_input(input);
	(14, (pt1(&rock), pt2(&rock))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Debug, Clone, Copy)]
enum Tile {
	Rock,
	Sand,
}

#[derive(Debug)]
struct Cave {
	points: BTreeMap<(i32, i32), Tile>,
	y_max: i32,
}

fn read_input(input: &str) -> Cave {
	fn parse_point(point_str: &str) -> Option<(i32, i32)> {
		let mut comps = point_str.split(',').array_chunks();
		let [x, y] = comps.next()?.map(|s| s.parse().ok());
		let [x, y] = [x?, y?];
		Some((x, y))
	}

	let mut max_y = i32::MIN;

	let points = input
		.lines()
		.chain(std::iter::once(""))
		.filter_map(|line| {
			let mut points = Vec::new();
			let mut comps = line.split_whitespace();
			let mut this_point = parse_point(comps.next()?)?;
			for next_point in comps.filter_map(parse_point) {
				let (x0, y0) = this_point;
				let (x1, y1) = next_point;
				this_point = next_point;

				let [x0, x1] = {
					let mut xs = [x0, x1];
					xs.sort_unstable();
					xs
				};

				let [y0, y1] = {
					let mut ys = [y0, y1];
					ys.sort_unstable();
					ys
				};

				max_y = max_y.max(y0);

				if x0 == x1 {
					for y in y0..=y1 {
						points.push(((x0, y), Tile::Rock));
					}
				} else {
					assert_eq!(y0, y1);
					let [x0, x1] = {
						let mut xs = [x0, x1];
						xs.sort_unstable();
						xs
					};
					for x in x0..=x1 {
						points.push(((x, y0), Tile::Rock));
					}
				}
			}
			Some(points)
		})
		.flatten()
		.collect::<_>();

	Cave {
		points,
		y_max: max_y,
	}
}

fn drop_sand(cave: &Cave, has_floor: bool) -> usize {
	let Cave {
		points,
		y_max: max_y,
	} = cave;
	let y_max = if has_floor { *max_y + 2 } else { *max_y };

	let mut points = points.clone();

	let mut n_sands = 0;
	loop {
		let mut x = 500;
		let mut y = 0;

		// Let the current grain of sand fall until it either can't fall further or falls
		// past the lowest (largest y) rock
		while y <= y_max {
			y += 1;
			let hit_bottom = has_floor && y == y_max;

			if !points.contains_key(&(x, y)) && !hit_bottom {
			} else if !points.contains_key(&(x - 1, y)) && !hit_bottom {
				x -= 1;
			} else if !points.contains_key(&(x + 1, y)) && !hit_bottom {
				x += 1;
			} else {
				// Couldn't move; undo the increment of y and break
				y -= 1;
				break;
			}
		}

		// The grain of sand didn't hit an obstacle, it'll just keep falling forever
		if y >= y_max {
			break;
		}

		points.insert((x, y), Tile::Sand);
		n_sands += 1;

		// The grain of sand didn't move from the source
		if (x, y) == (500, 0) {
			break;
		}
	}

	n_sands
}

// For debugging
#[allow(dead_code)]
fn map_to_string(map: &BTreeMap<(i32, i32), Tile>) -> String {
	let [mut x_min, mut y_min] = [i32::MAX; 2];
	let [mut x_max, mut y_max] = [i32::MIN; 2];

	for &(x, y) in map.keys() {
		if x < x_min {
			x_min = x;
		} else if x > x_max {
			x_max = x;
		}

		if y < y_min {
			y_min = y;
		} else if y > y_max {
			y_max = y;
		}
	}

	let mut s = String::new();
	for y in y_min..=y_max {
		for x in x_min..=x_max {
			let c = match map.get(&(x, y)) {
				None => '.',
				Some(Tile::Rock) => '#',
				Some(Tile::Sand) => 'O',
			};
			s.push(c);
		}
		s.push('\n');
	}
	s
}
// end::setup[]

// tag::pt1[]
fn pt1(cave: &Cave) -> usize {
	drop_sand(cave, false)
}
// end::pt1[]

// tag::pt2[]
fn pt2(cave: &Cave) -> usize {
	drop_sand(cave, true)
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
			&read_input(include_str!("sample_input.txt")),
			(pt1, 24),
			(pt2, 93),
		);
		run_tests(
			&read_input(include_str!("input.txt")),
			(pt1, 913),
			(pt2, 30762),
		);
	}
}
