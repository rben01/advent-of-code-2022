// tag::setup[]
use crate::{Answer, AocResult};
use std::collections::{BTreeMap, BTreeSet};

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	(3, (pt1(input), pt2(input))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

type CharSet = BTreeSet<char>;
struct Backpack(BTreeMap<char, usize>, BTreeMap<char, usize>);

fn read_input_pt1(input: &str) -> Vec<Backpack> {
	let mut compartments = Vec::new();
	for line in input.lines() {
		let compartment_size = line.len() / 2_usize;
		let mut chars = line.chars();

		let mut c1 = BTreeMap::new();
		for c in chars.by_ref().take(compartment_size) {
			*c1.entry(c).or_insert(0) += 1_usize;
		}

		let mut c2 = BTreeMap::new();
		for c in chars {
			*c2.entry(c).or_insert(0) += 1_usize;
		}

		compartments.push(Backpack(c1, c2));
	}

	compartments
}

fn read_input_pt2(input: &str) -> AocResult<Vec<(CharSet, CharSet, CharSet)>> {
	let mut backpacks = Vec::new();
	let mut lines = input.lines();
	for group in lines.by_ref().array_chunks() {
		let [s1, s2, s3] = group.map(|bp| bp.chars().collect::<BTreeSet<_>>());

		backpacks.push((s1, s2, s3));
	}

	if lines.count() != 0 {
		return Err("number of lines wasn't divisible by 3".into());
	}

	Ok(backpacks)
}

impl Backpack {
	fn overlap(&self) -> Vec<char> {
		let Self(first, second) = self;
		first
			.keys()
			.copied()
			.collect::<BTreeSet<_>>()
			.intersection(&second.keys().copied().collect())
			.copied()
			.collect()
	}
}

fn char_score(c: char) -> usize {
	let score = 1 + match c {
		'a'..='z' => (c as u8) - b'a',
		'A'..='Z' => (c as u8) - b'A' + 26,
		_ => panic!("invalid char {c:?}"),
	};
	score as _
}
// end::setup[]

// tag::pt1[]
fn pt1(input: &str) -> usize {
	let backpacks = read_input_pt1(input);

	let mut sum = 0;
	for backpack in backpacks {
		let overlap = backpack.overlap();
		for c in overlap {
			sum += char_score(c);
		}
	}
	sum
}
// end::pt1[]

// tag::pt2[]
fn pt2(input: &str) -> usize {
	let groups = read_input_pt2(input).expect("failed to read input");

	let mut sum = 0;
	for (g1, g2, g3) in groups {
		let shared_char = g1
			.intersection(&g2)
			.copied()
			.collect::<BTreeSet<_>>()
			.intersection(&g3)
			.copied()
			.next()
			.expect("no intersection in group");

		sum += char_score(shared_char);
	}

	sum
}
// end::pt2[]

#[cfg(test)]
mod test {
	#![allow(unused_imports)]
	use super::*;
	use crate::{test_part, test_parts};

	#[test]
	fn test() {
		test_parts(include_str!("sample_input.txt"), (pt1, 157), (pt2, 70));
		test_parts(include_str!("input.txt"), (pt1, 8053), (pt2, 2425));
	}
}
