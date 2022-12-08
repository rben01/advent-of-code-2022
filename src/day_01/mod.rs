// tag::setup[]
use crate::Answer;

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let elves = read_input(input).expect("could not read input");
	(1, (pt1(&elves), pt2(&elves))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

fn read_input(input: &str) -> Option<Vec<usize>> {
	let mut elves = Vec::new();
	let mut running_cals = 0;
	for line in input.lines().chain(std::iter::once("")) {
		let line = line.trim();
		if line.is_empty() {
			if running_cals > 0 {
				elves.push(running_cals);
			}
			running_cals = 0;
		} else {
			let cals = line.parse::<usize>().ok()?;
			running_cals += cals;
		}
	}

	Some(elves)
}
// end::setup[]

// tag::pt1[]
fn pt1(elves: &[usize]) -> usize {
	elves.iter().copied().fold(0, std::cmp::Ord::max)
}
// end::pt1[]

// tag::pt2[]
fn pt2(elves: &[usize]) -> usize {
	use std::collections::BinaryHeap;
	let heap = elves.iter().copied().collect::<BinaryHeap<_>>();
	heap.into_iter_sorted().take(3).sum()
}
// end::pt2[]

#[cfg(test)]
mod test {
	#![allow(unused_imports)]

	use super::*;
	use crate::{run_test, TestCase};

	#[test]
	fn test() {
		run_test(
			read_input(include_str!("sample_input.txt"))
				.unwrap()
				.as_slice(),
			((pt1, 24000), (pt2, 45000)),
		);
		run_test(
			read_input(include_str!("input.txt")).unwrap().as_slice(),
			((pt1, 64929), (pt2, 193_697)),
		);
	}
}
