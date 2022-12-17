// tag::setup[]
use crate::Answer;
use std::collections::BTreeSet;

fn ans_for_input(input: &str) -> Answer<i64, i64> {
	let map = read_input(input).expect("could not read input");
	(15, (pt1(&map), pt2(&map))).into()
}

pub fn ans() -> Answer<i64, i64> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Debug, Clone, Copy)]
struct Point {
	x: i64,
	y: i64,
}

impl Point {
	fn manhattan_dist(self, other: Self) -> i64 {
		(self.x - other.x).abs() + (self.y - other.y).abs()
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Interval {
	left: i64,
	right: i64,
}

impl Interval {
	fn width(self) -> i64 {
		self.right - self.left + 1
	}

	fn intersects(self, other: Self) -> bool {
		self.left <= other.right && self.right >= other.left
	}

	fn union_with_intersecting_interval(self, other: Self) -> Self {
		Self {
			left: self.left.min(other.left),
			right: self.right.max(other.right),
		}
	}
}

#[derive(Debug, Clone, Copy)]
enum RangeInclusive {
	Empty,
	Interval(Interval),
}

#[derive(Debug)]
struct Intervals {
	intervals: BTreeSet<Interval>,
}

impl Intervals {
	fn new() -> Self {
		Self {
			intervals: BTreeSet::new(),
		}
	}

	fn net_width(&self) -> i64 {
		self.intervals.iter().map(|interval| interval.width()).sum()
	}

	fn insert(&mut self, mut interval_to_insert: Interval) {
		let intersecting_intervals = self
			.intervals
			.iter()
			.filter_map(|&interval| interval.intersects(interval_to_insert).then_some(interval))
			.collect::<Vec<_>>();

		for interval in intersecting_intervals {
			interval_to_insert = interval_to_insert.union_with_intersecting_interval(interval);
			assert!(self.intervals.remove(&interval));
		}

		self.intervals.insert(interval_to_insert);
	}

	fn subtract_from(&self, interval: Interval) -> Intervals {
		let mut remaining_intervals = BTreeSet::new();

		let mut subtractor_prev_left = interval.left;
		// Note that self.intervals is sorted due to being a BTreeSet
		for &interval_to_subtract in &self.intervals {
			let Interval { left, right } = interval_to_subtract;
			// should always be true except maybe for the first iteration
			if left > subtractor_prev_left {
				remaining_intervals.insert(Interval {
					left: subtractor_prev_left,
					right: left - 1,
				});
			}
			subtractor_prev_left = right + 1;
		}

		Intervals {
			intervals: remaining_intervals,
		}
	}
}

#[derive(Debug, Clone, Copy)]
struct Sensor {
	sensor: Point,
	beacon: Point,
}

impl Sensor {
	fn sensor_dist_to(self, point: Point) -> i64 {
		self.sensor.manhattan_dist(point)
	}

	fn beacon_dist(self) -> i64 {
		self.sensor_dist_to(self.beacon)
	}

	fn row_extent_without_beacons(self, row: i64, exclude_own_beacon: bool) -> RangeInclusive {
		let row_dist = (row - self.sensor.y).abs();
		let half_width = self.beacon_dist() - row_dist;
		if half_width < 0 {
			RangeInclusive::Empty
		} else {
			let sensor_x = self.sensor.x;
			let left = sensor_x - half_width;
			let right = sensor_x + half_width;

			let interval = if exclude_own_beacon && row == self.beacon.y {
				if self.beacon.x < sensor_x {
					Interval {
						left: left + 1,
						right,
					}
				} else {
					Interval {
						left,
						right: right - 1,
					}
				}
			} else {
				Interval { left, right }
			};
			RangeInclusive::Interval(interval)
		}
	}
}

#[derive(Debug)]
struct Map {
	sensors: Vec<Sensor>,
	pt1_row_num: i64,
	max_coord: i64,
}

fn read_input(input: &str) -> Option<Map> {
	let mut sensors = Vec::new();
	let sensor_re = crate::regex!(
		r"Sensor at x=(?P<sensor_x>-?\d+), y=(?P<sensor_y>-?\d+): closest beacon is at x=(?P<beacon_x>-?\d+), y=(?P<beacon_y>-?\d+)"
	);

	let mut lines = input.lines();
	let pt1_row_num = lines.next()?.parse().ok()?;
	let max_coord = lines.next()?.parse().ok()?;

	for line in lines {
		let caps = sensor_re.captures(line)?;
		let [sensor_x, sensor_y, beacon_x, beacon_y] =
			["sensor_x", "sensor_y", "beacon_x", "beacon_y"].map(|name| {
				caps.name(name)
					.and_then(|value| value.as_str().parse().ok())
			});

		sensors.push(Sensor {
			sensor: Point {
				x: sensor_x?,
				y: sensor_y?,
			},
			beacon: Point {
				x: beacon_x?,
				y: beacon_y?,
			},
		});
	}

	Some(Map {
		sensors,
		pt1_row_num,
		max_coord,
	})
}

fn get_excluded_locations_in_row(
	map: &Map,
	row_num: i64,
	exclude_own_beacon: bool,
	clamp: bool,
) -> Intervals {
	let Map {
		sensors, max_coord, ..
	} = map;

	let max_coord = *max_coord;

	let mut excluded_intervals = Intervals::new();
	for sensor in sensors {
		let excluded_range = sensor.row_extent_without_beacons(row_num, exclude_own_beacon);

		if let RangeInclusive::Interval(Interval {
			mut left,
			mut right,
		}) = excluded_range
		{
			if clamp {
				left = left.max(0);
				right = right.min(max_coord);
			}

			excluded_intervals.insert(Interval { left, right });
		}
	}

	excluded_intervals
}
// end::setup[]

// tag::pt1[]
fn pt1(map: &Map) -> i64 {
	get_excluded_locations_in_row(map, map.pt1_row_num, true, false).net_width()
}
// end::pt1[]

// tag::pt2[]
fn pt2(map: &Map) -> i64 {
	let &Map { max_coord, .. } = map;

	let row_interval = Interval {
		left: 0,
		right: max_coord,
	};
	let row_width = row_interval.width();

	for row in 0..=max_coord {
		let excluded_locations = get_excluded_locations_in_row(map, row, false, true);
		if excluded_locations.net_width() < row_width {
			let missing = excluded_locations.subtract_from(row_interval);
			let x = missing.intervals.iter().next().unwrap().left;
			return x * 4_000_000 + row;
		}
	}

	panic!("could not find an empty location")
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
			(pt1, 26),
			(pt2, 56_000_011),
		);
		run_tests(
			&read_input(include_str!("input.txt")).unwrap(),
			(pt1, 5_142_231),
			(pt2, 10_884_459_367_718),
		);
	}
}
