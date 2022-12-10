// tag::setup[]
use crate::Answer;

fn ans_for_input(input: &str) -> Answer<i32, String> {
	let mat = read_input(input).expect("couldn't read input");
	(10, (pt1(&mat), pt2(&mat))).into()
}

pub fn ans() -> Answer<i32, String> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
	Noop,
	AddX(i32),
}

impl Instruction {
	fn n_cycles(self) -> usize {
		use Instruction::*;
		match self {
			Noop => 1,
			AddX(_) => 2,
		}
	}
}

impl TryFrom<&str> for Instruction {
	type Error = String;
	fn try_from(value: &str) -> Result<Self, Self::Error> {
		use Instruction::*;

		let mut comps = value.split_whitespace();
		let instr_name = comps
			.next()
			.ok_or_else(|| "could not get instruction".to_owned())?;
		let instruction = match instr_name {
			"noop" => Noop,
			"addx" => {
				let value = comps
					.next()
					.ok_or_else(|| "could not get value for addx".to_owned())?
					.parse()
					.map_err(|e| format!("{e:?}"))?;
				AddX(value)
			}
			_ => return Err(format!("invalid instruction {value:?}")),
		};
		Ok(instruction)
	}
}

fn read_input(input: &str) -> Option<Cpu> {
	let instructions = input
		.lines()
		.map(Instruction::try_from)
		.collect::<Result<Vec<_>, _>>()
		.ok()?;
	Some(Cpu { instructions })
}

struct Cpu {
	instructions: Vec<Instruction>,
}

impl Cpu {
	fn iter(&self) -> CpuIter<'_> {
		CpuIter {
			cpu: self,
			register: 1,
			index: 0,
			n_cycles: 0,
			loaded: false,
		}
	}
}

struct CpuIter<'a> {
	cpu: &'a Cpu,
	register: i32,
	index: usize,
	n_cycles: usize,
	loaded: bool,
}

impl Iterator for CpuIter<'_> {
	type Item = i32;
	fn next(&mut self) -> Option<Self::Item> {
		use Instruction::*;

		let Self {
			cpu: Cpu { instructions },
			register,
			index,
			n_cycles,
			loaded,
		} = self;

		let ret = *register;

		// before cycle
		let instruction = instructions.get(*index)?;
		if !*loaded && *n_cycles == 0 {
			*n_cycles += instruction.n_cycles();
			*loaded = true;
		}

		// during cycle
		if *n_cycles > 0 {
			*n_cycles -= 1;
		} else {
			match instruction {
				Noop => {}
				AddX(value) => *register += *value,
			}
			*loaded = false;
			*index += 1;
		}

		Some(ret)
	}
}

// end::setup[]

// tag::pt1[]
fn pt1(cpu: &Cpu) -> i32 {
	for (i, x) in (1..).zip(cpu.iter()) {
		println!("{:?}", (i, x));
	}

	(1..)
		.zip(cpu.iter())
		.skip(19)
		.step_by(40)
		.map(|(i, x)| i * x)
		.sum()
}
// end::pt1[]

// tag::pt2[]
fn pt2(cpu: &Cpu) -> String {
	const LEN: usize = 240;
	let mut grid = [false; LEN];
	for (i, (x, g)) in cpu.iter().zip(grid.iter_mut()).enumerate() {
		if (i32::try_from(i % 40).expect("couldn't convert {i} to i32") - x).abs() <= 1 {
			*g = true;
		}
	}

	let mut s = String::with_capacity(LEN);
	for (i, pixel) in grid.into_iter().enumerate() {
		if i > 0 && i % 40 == 0 {
			s.push('\n');
		}
		let c = if pixel { '#' } else { ' ' };
		s.push(c);
	}
	println!("{s}");
	s
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
			&read_input(include_str!("sample_input_1.txt")).unwrap(),
			(pt1, 13),
			(pt2, "a"),
		);
		run_tests(
			&read_input(include_str!("input.txt")).unwrap(),
			(pt1, 6256),
			(pt2, "x"),
		);
	}
}
