// tag::setup[]
use crate::{read_file, Answer};
use std::fmt::{Display, Write};

type Num = i64;
type Ans = String;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let number = read_input(input);
	(25, (pt1(&number), pt2(&number))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("input.txt"))
}

const FIVE: Num = 5;

#[derive(Debug, Clone, Copy)]
enum Digit {
	MinusTwo,
	MinusOne,
	Zero,
	PlusOne,
	PlusTwo,
}

impl Digit {
	fn value(self) -> Num {
		use Digit::*;
		match self {
			MinusTwo => -2,
			MinusOne => -1,
			Zero => 0,
			PlusOne => 1,
			PlusTwo => 2,
		}
	}
}

impl TryFrom<Num> for Digit {
	type Error = String;

	fn try_from(value: Num) -> Result<Self, Self::Error> {
		use Digit::*;
		Ok(match value {
			-2 => MinusTwo,
			-1 => MinusOne,
			0 => Zero,
			1 => PlusOne,
			2 => PlusTwo,
			_ => return Err(format!("could not convert {value:?} to a Digit")),
		})
	}
}

impl TryFrom<char> for Digit {
	type Error = String;

	fn try_from(value: char) -> Result<Self, Self::Error> {
		use Digit::*;
		Ok(match value {
			'=' => MinusTwo,
			'-' => MinusOne,
			'0' => Zero,
			'1' => PlusOne,
			'2' => PlusTwo,
			_ => return Err(format!("char {value:?} cannot be converted to a Digit")),
		})
	}
}

impl From<Digit> for char {
	fn from(value: Digit) -> Self {
		use Digit::*;
		match value {
			MinusTwo => '=',
			MinusOne => '-',
			Zero => '0',
			PlusOne => '1',
			PlusTwo => '2',
		}
	}
}

/// `self.0[i]` is the `5^i`s place. E.g., `[0]` is the 1's place, `[1]` is the 5's
/// place, etc
#[derive(Debug, Clone)]
struct Number(Vec<Digit>);

impl Number {
	fn value(&self) -> Num {
		(0..)
			.zip(&self.0)
			.map(|(i, digit)| digit.value() * FIVE.pow(i))
			.sum()
	}
}

impl Display for Number {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for &digit in self.0.iter().rev() {
			f.write_char(digit.into())?;
		}
		Ok(())
	}
}

impl FromIterator<Digit> for Number {
	fn from_iter<T: IntoIterator<Item = Digit>>(iter: T) -> Self {
		Self(iter.into_iter().collect())
	}
}

impl Default for Number {
	fn default() -> Self {
		Self(Default::default())
	}
}

fn read_input(input: &str) -> Vec<Number> {
	input
		.lines()
		.map(|line| {
			line.chars()
				.rev()
				.map(|k| Digit::try_from(k).unwrap())
				.collect()
		})
		.collect()
}

fn num_to_b5(mut n: Num) -> Number {
	if n == 0 {
		return Number::default();
	}

	// First, get ordinary base 5 representation

	// Starts off with a 0 in the most significant place
	let mut normal_base_5_rep_left_sig = vec![0];
	let mut exp_5 = (0..)
		.find_map(|i| (FIVE.pow(i + 1) > n).then_some(i))
		.unwrap();

	loop {
		let pow_5 = FIVE.pow(exp_5);
		let p = n / pow_5;

		normal_base_5_rep_left_sig.push(p);
		n -= p * pow_5;

		if exp_5 == 0 {
			break;
		}

		exp_5 -= 1;
	}

	// Then convert to the weird base 5 representation
	let mut digits = Vec::with_capacity(normal_base_5_rep_left_sig.len());

	let mut carry = false;
	for mut p in normal_base_5_rep_left_sig.into_iter().rev() {
		p += Num::from(carry);
		(p, carry) = if p > 2 { (p - 5, true) } else { (p, false) };

		digits.push(Digit::try_from(p).unwrap());
	}

	while let Some(&Digit::Zero) = digits.last() {
		digits.pop();
	}

	digits.into_iter().collect()
}

// end::setup[]

// tag::pt1[]
fn pt1(numbers: &[Number]) -> Ans {
	let n = numbers //
		.iter()
		.map(|n| n.value())
		.sum::<Num>();

	format!("{}", num_to_b5(n))
}
// end::pt1[]

// tag::pt2[]
fn pt2(_: &[Number]) -> Ans {
	"Happy Advent of Code!".into()
}
// end::pt2[]

#[cfg(test)]
mod test {
	#![allow(unused_imports)]
	use super::*;
	use crate::run_test;

	#[test]
	fn test() {
		run_test(
			read_input(&read_file!("sample_input_2.txt")).as_slice(),
			(pt1, "2=-1=0".to_owned()),
		);
		run_test(
			read_input(&read_file!("input.txt")).as_slice(),
			(pt1, "2=112--220-=-00=-=20".to_owned()),
		);
	}
}
