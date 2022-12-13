// tag::setup[]
use crate::{utils::get_nsew_adjacent, Answer};
use ndarray::prelude::*;
use priority_queue::PriorityQueue;
use std::{cmp::Reverse, collections::BTreeSet};

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let grid = read_input(input);
	(12, (pt1(&grid), pt2(&grid))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Debug)]
struct Grid {
	grid: Array2<u8>,
	start: (usize, usize),
	end: (usize, usize),
}

fn read_input(input: &str) -> Grid {
	let mut heights = Vec::with_capacity(input.len());

	let mut n_rows = 0;
	let mut n_cols = 0;

	let mut start = None;
	let mut end = None;

	for (row, line) in input.lines().enumerate() {
		n_cols = line.len();
		n_rows += 1;

		for (col, c) in line.bytes().enumerate() {
			let height_char = match c {
				b'S' => {
					start = Some((row, col));
					b'a'
				}
				b'E' => {
					end = Some((row, col));
					b'z'
				}
				b'a'..=b'z' => c,
				_ => panic!("unexpected character {c:?}"),
			};
			let height = height_char - b'a';

			heights.push(height);
		}
	}

	let heights_len = heights.len();
	let grid = Array2::from_shape_vec((n_rows, n_cols), heights).unwrap_or_else(|_| {
		panic!(
			"could not make {n_rows} * {n_cols} array from Vec of len {}",
			heights_len
		)
	});
	Grid {
		grid,
		start: start.expect("did not find a starting square"),
		end: end.expect("did not find an ending square"),
	}
}

// Neat switcheroo for code reuse: In part 1, instead of navigating from S to E, we
// navigate from E to S and do the height comparison in the other direction. Then when
// we do part 2, we just don't navigate back to the start, but instead to any starting
// point.
fn search(grid: ArrayView2<u8>, start: (usize, usize), end: Option<(usize, usize)>) -> usize {
	let mut seen = BTreeSet::new();
	let mut pq = PriorityQueue::new();

	// key: location, height; priority: Reverse(path length)
	pq.push((start, grid[start]), Reverse(0));

	while let Some(((coords, height), Reverse(curr_len))) = pq.pop() {
		if let Some(end) = end {
			if coords == end {
				return curr_len;
			}
		} else if height == 0 {
			return curr_len;
		}

		seen.insert(coords);

		for adj_coords in get_nsew_adjacent(coords, (0, grid.nrows() - 1), (0, grid.ncols() - 1))
			.into_iter()
			.flatten()
		{
			let adj_height = grid[adj_coords];

			if seen.contains(&adj_coords) || (height >= 1 && adj_height < height - 1) {
				continue;
			}

			pq.push((adj_coords, adj_height), Reverse(curr_len + 1));
		}
	}

	panic!("could not find end")
}
// end::setup[]

// tag::pt1[]
fn pt1(grid: &Grid) -> usize {
	let Grid { grid, start, end } = grid;
	search(grid.view(), *end, Some(*start))
}
// end::pt1[]

// tag::pt2[]
fn pt2(grid: &Grid) -> usize {
	let Grid {
		grid,
		start: _,
		end,
	} = grid;
	search(grid.view(), *end, None)
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
			(pt1, 31),
			(pt2, 29),
		);
		run_tests(
			&read_input(include_str!("input.txt")),
			(pt1, 472),
			(pt2, 465),
		);
	}
}
