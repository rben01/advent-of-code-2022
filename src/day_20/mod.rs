use std::collections::HashMap;

// tag::setup[]
use crate::{read_file, Answer, Cast};

type Num = i64;
type Ans = Num;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Elem(Num, usize);

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let nums = read_input(input).expect("could not read input");
	(20, (pt1(nums.clone()), pt2(nums))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("input.txt"))
}

fn read_input(input: &str) -> Result<Vec<Elem>, String> {
	let mut counts = HashMap::new();
	input
		.lines()
		.map(|line| {
			let n = line.parse::<Num>().map_err(|e| e.to_string())?;
			let count = counts.entry(n).or_insert(0);
			let ans = Elem(n, *count);
			*count += 1;
			Ok(ans)
		})
		.collect()
}

fn mix(nums: &mut Vec<Elem>, orig_nums: &[Elem]) {
	let len = nums.len().cast::<Num>();
	let mut positions = nums.iter().copied().zip(0_i64..).collect::<HashMap<_, _>>();
	// println!("{:?}", nums.iter().map(|Elem(n, _)| *n).collect::<Vec<_>>());

	for &e @ Elem(n, _) in orig_nums {
		// dbg!("**");
		if n == 0 {
			continue;
		}

		let direction = if n < 0 { -1 } else { 1 };
		let mut i = positions[&e];

		// dbg!(n);
		// dbg!(n.abs().rem_euclid(len));
		// dbg!(i);
		// dbg!((i + direction).rem_euclid(len));
		// dbg!();

		for _ in 0..(n.abs().rem_euclid(len - 1)) {
			let prev = (i + direction).rem_euclid(len);
			let m = nums[prev.cast::<usize>()];

			nums[i.cast::<usize>()] = m;
			positions.insert(m, i).unwrap();

			i = prev;
		}

		nums[i.cast::<usize>()] = e;
		positions.insert(e, i).unwrap();

		// println!("{:?}", nums.iter().map(|Elem(n, _)| *n).collect::<Vec<_>>());
	}
}

fn get_ans(nums: &[Elem]) -> Ans {
	let index_of_0 = nums
		.iter()
		.position(|&Elem(n, _)| n == 0)
		.unwrap_or_else(|| panic!("could not find 0"));

	let len = nums.len();
	[1000, 2000, 3000]
		.into_iter()
		.map(|offset| nums[(index_of_0 + offset) % len].0)
		.sum()
}
// end::setup[]

// tag::pt1[]
fn pt1(mut nums: Vec<Elem>) -> Ans {
	let orig_nums = nums.clone();
	mix(&mut nums, &orig_nums);
	get_ans(&nums)
}
// end::pt1[]

// tag::pt2[]
fn pt2(nums: Vec<Elem>) -> Ans {
	let key = 811_589_153;
	let mut nums = nums
		.into_iter()
		.map(|Elem(n, c)| Elem(n * key, c))
		.collect::<Vec<_>>();
	let orig_nums = nums.clone();
	for _ in 0..10 {
		mix(&mut nums, &orig_nums);
	}
	get_ans(&nums)
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
			read_input(&read_file!("sample_input.txt")).unwrap(),
			(pt1, 3),
			(pt2, 0),
		);
		run_tests(
			read_input(&read_file!("input.txt")).unwrap(),
			(pt1, 3147),
			(pt2, 0),
		);
	}
}
