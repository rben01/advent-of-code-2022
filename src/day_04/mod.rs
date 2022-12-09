// tag::setup[]
use crate::{Answer, ToResultDefaultErr};

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let pairs = read_input(input).unwrap();
	(4, (pt1(&pairs), pt2(&pairs))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

struct Assignment {
	low: i32,
	high: i32,
}

impl TryFrom<&str> for Assignment {
	type Error = String;
	fn try_from(low_high: &str) -> Result<Self, Self::Error> {
		let mut comps = low_high.split('-');
		let [low, high] = [(); 2].map(|_| {
			comps
				.next()
				.to_result()?
				.parse::<i32>()
				.map_err(|e| format!("{e:?}"))
		});
		let [low, high] = [low?, high?];

		Ok(Self { low, high })
	}
}

struct Pair(Assignment, Assignment);

fn read_input(input: &str) -> Option<Vec<Pair>> {
	let mut pairs = Vec::new();
	for line in input.lines() {
		let mut comps = line.split(',');
		let s1 = comps.next()?;
		let s2 = comps.next()?;
		let [a1, a2] = [s1, s2].map(Assignment::try_from);
		let [a1, a2] = [a1.ok()?, a2.ok()?];
		pairs.push(Pair(a1, a2));
	}

	Some(pairs)
}

// end::setup[]

// tag::pt1[]
fn pt1(pairs: &[Pair]) -> usize {
	pairs
		.iter()
		.map(|Pair(a1, a2)| {
			let Assignment {
				low: low1,
				high: high1,
			} = a1;
			let Assignment {
				low: low2,
				high: high2,
			} = a2;

			let b = low1 <= low2 && high1 >= high2 || low2 <= low1 && high2 >= high1;
			usize::from(b)
		})
		.sum()
}
// end::pt1[]

// tag::pt2[]
fn pt2(pairs: &[Pair]) -> usize {
	pairs
		.iter()
		.map(|Pair(a1, a2)| {
			let Assignment {
				low: low1,
				high: high1,
			} = a1;
			let Assignment {
				low: low2,
				high: high2,
			} = a2;

			let b = high1 >= low2 && high2 >= low1;
			usize::from(b)
		})
		.sum()
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
			read_input(include_str!("sample_input.txt"))
				.unwrap()
				.as_slice(),
			(pt1, 2),
			(pt2, 4),
		);
		run_tests(
			read_input(include_str!("input.txt")).unwrap().as_slice(),
			(pt1, 518),
			(pt2, 909),
		);
	}
}
