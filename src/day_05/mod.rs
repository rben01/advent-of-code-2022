// tag::setup[]
use crate::Answer;

fn ans_for_input(input: &str) -> Answer<String, String> {
	(5, (pt1(input), pt2(input))).into()
}

pub fn ans() -> Answer<String, String> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Clone, Copy, Debug)]
struct Instruction {
	src: usize,
	dst: usize,
	count: usize,
}

#[derive(Debug)]
struct Piles(Vec<Vec<char>>);

#[derive(Clone, Copy, Debug)]
enum Model {
	M9000,
	M9001,
}

impl Piles {
	fn apply_one(&mut self, instr @ Instruction { src, dst, count }: Instruction, model: Model) {
		match model {
			Model::M9000 => {
				for _ in 0..count {
					let elem = self.0[src]
						.pop()
						.unwrap_or_else(|| panic!("invalid instruction {instr:?}"));
					self.0[dst].push(elem);
				}
			}
			Model::M9001 => {
				let src_pile = &mut self.0[src];
				let to_move = src_pile.drain(src_pile.len() - count..).collect::<Vec<_>>();
				self.0[dst].extend(to_move);
			}
		}
	}

	fn apply(&mut self, instrs: impl IntoIterator<Item = Instruction>, model: Model) {
		for instr in instrs {
			self.apply_one(instr, model);
		}
	}

	fn top(&self) -> String {
		self.0.iter().filter_map(|p| p.last()).collect()
	}
}

fn read_input(input: &str) -> Option<(Piles, Vec<Instruction>)> {
	let mut piles = Vec::new();
	let mut instructions = Vec::new();
	let mut lines = input.lines();

	for line in lines.by_ref() {
		if line.trim().is_empty() {
			break;
		}

		for (i, chunk) in line
			.chars()
			// pad line with ' ' to make the length of the final segment 4 (instead of only
			// 3, or 2 in the very last line)
			.chain(std::iter::once(' '))
			.chain(std::iter::once(' '))
			.array_chunks::<4>()
			.enumerate()
		{
			let c = chunk[1];

			if i >= piles.len() {
				piles.push(Vec::new());
			}

			if !c.is_whitespace() {
				piles[i].push(c);
			}
		}
	}

	for pile in &mut piles {
		pile.pop(); // remove the pile labels
		pile.reverse(); // since elems were added top to bottom
	}

	for line in lines {
		let mut comps = line.split_whitespace();

		comps.next()?;
		let count = comps.next()?.parse().ok()?;
		comps.next()?;
		// input is 1-indexed, we are 0-indexed
		let src = comps.next()?.parse::<usize>().ok()? - 1;
		comps.next()?;
		let dst = comps.next()?.parse::<usize>().ok()? - 1;

		instructions.push(Instruction { src, dst, count });
	}

	Some((Piles(piles), instructions))
}
// end::setup[]

// tag::pt1[]
fn pt1(input: &str) -> String {
	let (mut piles, instructions) = read_input(input).expect("could not parse input");
	piles.apply(instructions, Model::M9000);
	piles.top()
}
// end::pt1[]

// tag::pt2[]
fn pt2(input: &str) -> String {
	let (mut piles, instructions) = read_input(input).expect("could not parse input");
	piles.apply(instructions, Model::M9001);
	piles.top()
}
// end::pt2[]

#[cfg(test)]
mod test {
	#![allow(unused_imports)]
	use super::*;
	use crate::run_tests;

	#[test]
	fn test() {
		run_tests(include_str!("sample_input.txt"), (pt1, "CMZ"), (pt2, "MCD"));
		run_tests(
			include_str!("input.txt"),
			(pt1, "VJSFHWGFT"),
			(pt2, "LCTQFBVZV"),
		);
	}
}
