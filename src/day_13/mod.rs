// tag::setup[]
use crate::{
	utils::{into_rc_rc, RcRc},
	Answer,
};
use std::{cell::RefCell, rc::Rc, str::FromStr};

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let pp = read_input(input).expect("could not read input");
	(13, (pt1(&pp), pt2(&pp))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Debug)]
enum ListBuilder {
	Number(i32),
	Nested(Vec<Rc<RefCell<ListBuilder>>>),
}

impl FromStr for ListBuilder {
	type Err = String;
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		fn handle_running_num_str(
			num_str: &mut String,
			list_stack: &mut [RcRc<ListBuilder>],
		) -> Result<(), String> {
			if num_str.is_empty() {
				return Ok(());
			}

			let value = match num_str.parse() {
				Ok(n) => n,
				Err(_) => return Err(format!("could not parse {num_str:?} as an i32")),
			};
			num_str.clear();

			if let Some(last) = list_stack.last() {
				match &mut *last.borrow_mut() {
					ListBuilder::Number(..) => panic!("logic error: the stack contained a number"),
					ListBuilder::Nested(l) => l.push(into_rc_rc(ListBuilder::Number(value))),
				}
			}

			Ok(())
		}

		let mut list_stack = Vec::<RcRc<Self>>::new();
		let mut root = None;
		let mut running_num_str = String::new();

		for c in s.chars() {
			match c {
				'[' => {
					let list = into_rc_rc(Self::Nested(Vec::new()));
					if let Some(last) = list_stack.last() {
						match &mut *last.borrow_mut() {
							Self::Number(..) => {
								panic!("logic error: the stack contained a number")
							}
							Self::Nested(l) => l.push(Rc::clone(&list)),
						}
					}
					list_stack.push(list);
				}
				']' => {
					handle_running_num_str(&mut running_num_str, &mut list_stack)?;
					let last = match list_stack.pop() {
						Some(last) => last,
						None => return Err("unexpected closing brace".into()),
					};
					root = Some(last);
				}
				',' => {
					handle_running_num_str(&mut running_num_str, &mut list_stack)?;
				}
				'0' | '1'..='9' => {
					running_num_str.push(c);
				}
				' ' => {}
				_ => return Err(format!("invalid character {c:?}")),
			}
		}

		let res_rcrc =
			root.unwrap_or_else(|| panic!("logic error: did not have a root node at the end"));
		let res = Rc::try_unwrap(res_rcrc)
			.unwrap_or_else(|_| {
				panic!("logic error: the root had multiple outstanding references by the end of the function")
			})
			.into_inner();

		Ok(res)
	}
}

#[derive(Debug, PartialEq, Eq)]
enum List {
	Number(i32),
	Nested(Vec<List>),
}

impl From<ListBuilder> for List {
	fn from(value: ListBuilder) -> Self {
		match value {
			ListBuilder::Number(n) => Self::Number(n),
			ListBuilder::Nested(l) => Self::Nested(
				l.into_iter()
					.map(|rc| Rc::try_unwrap(rc).unwrap().into_inner().into())
					.collect(),
			),
		}
	}
}

impl FromStr for List {
	type Err = <ListBuilder as FromStr>::Err;
	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let builder = ListBuilder::from_str(s)?;
		Ok(Self::from(builder))
	}
}

impl PartialOrd for List {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for List {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		use List::*;
		match (self, other) {
			(Number(x1), Number(x2)) => x1.cmp(x2),
			(&Number(x1), Nested(l2)) => vec![Number(x1)].cmp(l2),
			(Nested(l1), &Number(x2)) => l1.cmp(&vec![Number(x2)]),
			(Nested(l1), Nested(l2)) => l1.cmp(l2),
		}
	}
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Packet(Vec<List>);

impl TryFrom<List> for Packet {
	type Error = String;
	fn try_from(value: List) -> Result<Self, Self::Error> {
		match value {
			List::Number(_) => Err("cannot construct a Packet from a List::Number".into()),
			List::Nested(l) => Ok(Self(l)),
		}
	}
}

#[derive(Debug)]
struct PacketPair(Packet, Packet);

impl PacketPair {
	fn is_sorted(&self) -> bool {
		self.0 < self.1
	}
}

fn read_input(input: &str) -> Option<Vec<PacketPair>> {
	input
		.lines()
		.chain(std::iter::once(""))
		.array_chunks()
		.map(|[l0, l1, _]| {
			let [p0, p1] =
				[l0, l1].map(|line| line.parse::<List>().and_then(Packet::try_from).ok());
			let [p0, p1] = [p0?, p1?];
			Some(PacketPair(p0, p1))
		})
		.collect::<_>()
}
// end::setup[]

// tag::pt1[]
fn pt1(pps: impl AsRef<[PacketPair]>) -> usize {
	(1_usize..)
		.zip(pps.as_ref())
		.filter_map(|(i, pp)| pp.is_sorted().then_some(i))
		.sum()
}
// end::pt1[]

// tag::pt2[]
fn pt2(pps: impl AsRef<[PacketPair]>) -> usize {
	let divider_packets = [2, 6].map(|n| {
		Packet::try_from(List::Nested(vec![List::Nested(vec![List::Number(n)])])).unwrap()
	});
	let mut packets = pps
		.as_ref()
		.iter()
		.flat_map(|PacketPair(p1, p2)| [p1, p2])
		.chain(&divider_packets)
		.collect::<Vec<_>>();
	packets.sort();

	let mut idx1 = None;
	let mut idx2 = None;
	for (i, &packet) in (1..).zip(&packets) {
		if packet == &divider_packets[0] {
			idx1 = Some(i);
		} else if packet == &divider_packets[1] {
			idx2 = Some(i);
		}
	}

	idx1.and_then(|i1| idx2.map(|i2| i1 * i2)).unwrap()
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
			&read_input(include_str!("sample_input.txt")).unwrap(),
			(pt1, 13),
			(pt2, 140),
		);
		run_tests(
			&read_input(include_str!("input.txt")).unwrap(),
			(pt1, 5625),
			(pt2, 23111),
		);
	}
}
