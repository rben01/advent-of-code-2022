// tag::setup[]
use crate::Answer;
use std::collections::{BTreeMap, VecDeque};

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	(6, (pt1(input), pt2(input))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

fn read_input(input: &str) -> &str {
	input.trim()
}

fn find_marker(chars: impl IntoIterator<Item = char>, marker_len: usize) -> Option<usize> {
	let mut char_iter = (1..).zip(chars);

	let mut buf = VecDeque::with_capacity(marker_len);
	let mut counts = BTreeMap::new();

	for _ in 0..marker_len {
		let (_, c) = char_iter.next()?;
		buf.push_back(c);
		*counts.entry(c).or_insert(0_usize) += 1;
	}

	for (i, c) in char_iter {
		let prev = buf.pop_front().unwrap();
		buf.push_back(c);

		let prev_count = counts.get_mut(&prev).unwrap();
		if *prev_count == 1 {
			counts.remove(&prev);
		} else {
			*prev_count -= 1;
		}

		*counts.entry(c).or_insert(0) += 1;

		if counts.keys().len() == marker_len {
			return Some(i);
		}
	}

	Some(0)
}
// end::setup[]

// tag::pt1[]
fn pt1(input: &str) -> usize {
	let chars = read_input(input).chars();
	find_marker(chars, 4).unwrap_or_else(|| panic!("could not get marker from {input:?}"))
}
// end::pt1[]

// tag::pt2[]
fn pt2(input: &str) -> usize {
	let chars = read_input(input).chars();
	find_marker(chars, 14).unwrap_or_else(|| panic!("could not get marker from {input:?}"))
}
// end::pt2[]

#[cfg(test)]
mod test {
	#![allow(unused_imports)]
	use super::*;
	use crate::{test_part, test_parts};

	#[test]
	fn test() {
		test_parts(include_str!("sample_input.txt"), (pt1, 7), (pt2, 19));
		test_parts(include_str!("input.txt"), (pt1, 1093), (pt2, 3534));
	}
}
