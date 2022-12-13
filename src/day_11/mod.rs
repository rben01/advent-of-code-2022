// tag::setup[]
use crate::Answer;
use std::{
	cmp::{Ordering, Reverse},
	collections::{BTreeMap, VecDeque},
	ops::{Add, AddAssign, Mul, MulAssign, Rem, Sub, SubAssign},
};

// Why 23? Because I looked at the inputs and that was the biggest divisor
type Part2ModuloTracker = ModuloTracker<23>;

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let monkeys1 = read_input::<i32>(input).expect("couldn't read input");
	let monkeys2 = read_input::<Part2ModuloTracker>(input).expect("couldn't read input");

	(11, (pt1(monkeys1), pt2(monkeys2))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

trait NumberLike:
	Copy
	+ From<i32>
	+ Add<i32, Output = Self>
	+ Sub<i32, Output = Self>
	+ Mul<i32, Output = Self>
	+ Rem<i32, Output = i32>
	+ Add<Self, Output = Self>
	+ Sub<Self, Output = Self>
	+ Mul<Self, Output = Self>
	+ AddAssign<i32>
	+ MulAssign<i32>
	+ SubAssign<i32>
{
	fn maybe_div_by_three(self) -> Self;
}

impl<const N: usize> NumberLike for ModuloTracker<N> {
	fn maybe_div_by_three(self) -> Self {
		self
	}
}

impl NumberLike for i32 {
	fn maybe_div_by_three(self) -> Self {
		self / 3
	}
}

/// The crux of part 2 of this problem is that numbers can grow without bound. However
/// all we need to know about a number in order to know which monkey it will be thrown
/// to is whether it is divisible by a given M. To do this, we simply store the numbder
/// modulo every number up to the maximum divisor we'll encounter (for my input, 23).
///
/// When computing an operation on this number, we just use modular arithmetic to
/// compute its new remainders modulo all of the numbers. Then the divisibility checks
/// just require checking whether `remainders[denom-1] == 0`.
///
/// **Panics** `%` will panic if the divisor is larger than `N`.
#[derive(Copy, Clone)]
struct ModuloTracker<const N: usize> {
	/// The remainders of the current number. `data[i-1]` is the represented number
	/// modulo `i`. (In theory you could use the Chinese remainder theorem to figure out
	/// the smallest number that an instance of this could represent.)
	remainders: [i32; N],
}

fn value_to_index(value: i32) -> usize {
	usize::try_from(value).unwrap() - 1
}

fn index_to_value(index: usize) -> i32 {
	i32::try_from(index).unwrap() + 1
}

impl<const N: usize> ModuloTracker<N> {
	fn clamp(&mut self) {
		for i in 0..N {
			self.remainders[i] %= index_to_value(i);
		}
	}
}

impl<const N: usize> From<i32> for ModuloTracker<N> {
	fn from(value: i32) -> Self {
		let mut data = [0; N];
		for (i, datum) in data.iter_mut().enumerate() {
			let denom = index_to_value(i);
			*datum = value % denom;
		}
		Self { remainders: data }
	}
}

macro_rules! impl_op_i32_for_ModuloTracker {
	($trait_name:ident, $method_name:ident, $operator:tt) => {
		impl<const N: usize> $trait_name<i32> for ModuloTracker<N> {
			type Output = Self;
			fn $method_name(self, rhs: i32) -> Self::Output {
				let mut remainders = [0; N];
				for (i, (r0, r1)) in self.remainders.into_iter().zip(&mut remainders).enumerate() {
					*r1 = (r0 $operator rhs) % index_to_value(i);
				}

				let mut ans = Self { remainders };
				ans.clamp();
				ans
			}
		}
	};
}

impl_op_i32_for_ModuloTracker!(Add, add, +);
impl_op_i32_for_ModuloTracker!(Sub, sub, -);
impl_op_i32_for_ModuloTracker!(Mul, mul, *);

macro_rules! impl_op_ModuloTracker_for_ModuloTracker {
	 ($trait_name:ident, $method_name:ident, $operator:tt) => {
		impl<const N: usize> $trait_name<ModuloTracker<N>> for ModuloTracker<N> {
			type Output = Self;
			fn $method_name(self, rhs: ModuloTracker<N>) -> Self::Output {
				let mut remainders = [0; N];
				for ((r0, r1), r2) in self.remainders.into_iter().zip(rhs.remainders).zip(&mut remainders) {
					*r2 = r0 $operator r1
				}

				let mut ans = Self { remainders };
				ans.clamp();
				ans
			}
		}
	};
}

impl_op_ModuloTracker_for_ModuloTracker!(Add, add, +);
impl_op_ModuloTracker_for_ModuloTracker!(Sub, sub, -);
impl_op_ModuloTracker_for_ModuloTracker!(Mul, mul, *);

macro_rules! impl_op_assign_for_ModuloTracker {
	($trait_name:ident, $method_name:ident, $operator:tt) => {
		impl<const N: usize> $trait_name<i32> for ModuloTracker<N> {
			fn $method_name(&mut self, rhs: i32) {
				*self = *self $operator rhs;
			}
		}
	};
}

impl_op_assign_for_ModuloTracker!(AddAssign, add_assign, +);
impl_op_assign_for_ModuloTracker!(SubAssign, sub_assign, -);
impl_op_assign_for_ModuloTracker!(MulAssign, mul_assign, *);

impl<const N: usize> Rem<i32> for ModuloTracker<N> {
	type Output = i32;
	fn rem(self, rhs: i32) -> Self::Output {
		self.remainders[value_to_index(rhs)]
	}
}

#[derive(Debug, Clone, Copy)]
enum Operand<T: NumberLike> {
	OldValue,
	Literal(T),
}

impl<T: NumberLike> Operand<T> {
	fn substitute_for(self, value: T) -> T {
		use Operand::*;
		match self {
			OldValue => value,
			Literal(x) => x,
		}
	}
}

impl<T: NumberLike> TryFrom<&str> for Operand<T> {
	type Error = String;
	fn try_from(value: &str) -> Result<Self, Self::Error> {
		let operand = match value {
			"old" => Operand::OldValue,
			_ => Operand::Literal(
				value
					.parse::<i32>()
					.map(T::from)
					.map_err(|_| format!("could not convert {value:?} to an Operand"))?,
			),
		};
		Ok(operand)
	}
}

#[derive(Debug, Clone, Copy)]
enum Operator {
	Add,
	Sub,
	Mul,
}

impl TryFrom<&str> for Operator {
	type Error = String;
	fn try_from(value: &str) -> Result<Self, Self::Error> {
		use Operator::*;
		let operator = match value {
			"+" => Add,
			"-" => Sub,
			"*" => Mul,
			_ => return Err("could not convert {value:?} to an Operator".into()),
		};
		Ok(operator)
	}
}

#[derive(Debug, Clone, Copy)]
struct Operation<T: NumberLike> {
	operator: Operator,
	operands: (Operand<T>, Operand<T>),
}

impl<T: NumberLike> Operation<T> {
	fn apply(self, value: T) -> T {
		use Operator::*;

		let Self {
			operator,
			operands: (op_0, op_1),
		} = self;
		let left = op_0.substitute_for(value);
		let right = op_1.substitute_for(value);

		match operator {
			Add => left + right,
			Sub => left - right,
			Mul => left * right,
		}
	}
}

#[derive(Debug, Clone, Copy)]
enum TestPredicate {
	DivisibleBy(i32),
}

#[derive(Debug, Clone, Copy)]
struct Test {
	predicate: TestPredicate,
	dest_if_true: usize,
	dest_if_false: usize,
}

impl Test {
	fn get_dest(self, value: impl NumberLike) -> usize {
		let Self {
			predicate,
			dest_if_true,
			dest_if_false,
		} = self;

		match predicate {
			TestPredicate::DivisibleBy(n) => {
				if value % n == 0 {
					dest_if_true
				} else {
					dest_if_false
				}
			}
		}
	}
}

#[derive(Debug, Clone)]
struct Monkey<T: NumberLike> {
	index: usize,
	items: VecDeque<T>,
	operation: Operation<T>,
	test: Test,
}

#[derive(Debug, Clone)]
struct Monkeys<T: NumberLike> {
	monkeys: Vec<Monkey<T>>,
	inspection_counts: BTreeMap<usize, usize>,
}

impl<T: NumberLike> Monkeys<T> {
	fn run_round(&mut self) {
		for i in 0..self.monkeys.len() {
			// sigh...
			let (head, tail_incl_this_monkey) = self.monkeys.split_at_mut(i);
			let (this_monkey_singleton, tail) = tail_incl_this_monkey.split_at_mut(1);
			let this_monkey = &mut this_monkey_singleton[0];

			while let Some(worry_level) = this_monkey.items.pop_front() {
				*self.inspection_counts.entry(i).or_insert(0) += 1;

				let worry_level = this_monkey
					.operation
					.apply(worry_level)
					.maybe_div_by_three();
				let dest_monkey_index = this_monkey.test.get_dest(worry_level);
				let dest_monkey = match dest_monkey_index.cmp(&i) {
					Ordering::Less => &mut head[dest_monkey_index],
					Ordering::Equal => panic!("monkey {i} unexpectedly threw an item to itself"),
					Ordering::Greater => &mut tail[dest_monkey_index - i - 1],
				};

				dest_monkey.items.push_back(worry_level);
			}
		}
	}
}

fn read_input<T: NumberLike>(input: &str) -> Option<Monkeys<T>> {
	let mut monkeys = Vec::new();

	let line_chunks = input.lines().chain(std::iter::once("")).array_chunks();
	for [l0, l1, l2, l3, l4, l5, _] in line_chunks {
		let index = l0
			.split_whitespace()
			.nth(1)?
			.trim_end_matches(':')
			.parse()
			.ok()?;

		let items = l1
			.split_whitespace()
			.skip(2)
			.map(|s| s.trim_matches(',').parse::<i32>().ok().map(T::from))
			.collect::<Option<_>>()?;

		let mut operation_tokens = l2.split_whitespace().skip(3);
		let operand_0 = Operand::try_from(operation_tokens.next()?).ok()?;
		let operator = Operator::try_from(operation_tokens.next()?).ok()?;
		let operand_1 = Operand::try_from(operation_tokens.next()?).ok()?;

		let test_denominator = l3.split_whitespace().nth(3)?.parse().ok()?;

		let dest_if_true = l4.split_whitespace().nth(5)?.parse().ok()?;
		let dest_if_false = l5.split_whitespace().nth(5)?.parse().ok()?;

		let monkey = Monkey {
			index,
			items,
			operation: Operation {
				operator,
				operands: (operand_0, operand_1),
			},
			test: Test {
				predicate: TestPredicate::DivisibleBy(test_denominator),
				dest_if_true,
				dest_if_false,
			},
		};

		monkeys.push(monkey);
	}

	monkeys.sort_by_key(|m| m.index);

	Some(Monkeys {
		monkeys,
		inspection_counts: BTreeMap::new(),
	})
}

fn get_monkey_business<T: NumberLike>(mut monkeys: Monkeys<T>, n_rounds: usize) -> usize {
	for _ in 0..n_rounds {
		monkeys.run_round();
	}
	let Monkeys {
		inspection_counts, ..
	} = monkeys;
	let mut inspection_counts = inspection_counts.into_values().collect::<Vec<_>>();
	inspection_counts.sort_unstable_by_key(|count| Reverse(*count));

	inspection_counts[0] * inspection_counts[1]
}
// end::setup[]

// tag::pt1[]
fn pt1<T: NumberLike>(monkeys: Monkeys<T>) -> usize {
	get_monkey_business(monkeys, 20)
}
// end::pt1[]

// tag::pt2[]
fn pt2<T: NumberLike>(monkeys: Monkeys<T>) -> usize {
	get_monkey_business(monkeys, 10000)
}
// end::pt2[]

#[cfg(test)]
mod test {
	#![allow(unused_imports)]
	use super::*;
	use crate::{run_test, run_tests};

	#[test]
	fn test() {
		{
			let input = include_str!("sample_input.txt");
			run_test(read_input::<i32>(input).unwrap(), (pt1, 10605));
			run_test(
				read_input::<Part2ModuloTracker>(input).unwrap(),
				(pt2, 2_713_310_158),
			);
		}

		{
			let input = include_str!("input.txt");
			run_test(read_input::<i32>(input).unwrap(), (pt1, 57838));
			run_test(
				read_input::<Part2ModuloTracker>(input).unwrap(),
				(pt2, 15_050_382_231),
			);
		}
	}
}
