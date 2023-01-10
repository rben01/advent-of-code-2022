// tag::setup[]
use crate::{read_file, regex, Answer, ToResultDefaultErr};
use std::{
	collections::HashMap,
	fmt::{Display, Write},
	str::FromStr,
};

type Num = i64;
type Ans = Num;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let nums = read_input(input).expect("could not read input");
	(21, (pt1(nums.clone()), pt2(nums))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("input.txt"))
}

#[derive(Debug, Clone, Copy)]
enum Operator {
	Add,
	Sub,
	Mul,
	Div,
}

impl Operator {
	fn apply(self, x: Num, y: Num) -> Num {
		use Operator::*;
		match self {
			Add => x + y,
			Sub => x - y,
			Mul => x * y,
			Div => x / y,
		}
	}
}

impl Display for Operator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		use Operator::*;
		f.write_char(match self {
			Add => '+',
			Sub => '-',
			Mul => '*',
			Div => '/',
		})
	}
}

impl FromStr for Operator {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		use Operator::*;
		let ans = match s {
			"+" => Add,
			"-" => Sub,
			"*" => Mul,
			"/" => Div,
			_ => return Err(format!("could not parse {s:?} into an Operator")),
		};
		Ok(ans)
	}
}

#[derive(Debug, Clone)]
struct Computation {
	operator: Operator,
	a: String,
	b: String,
}

#[derive(Debug, Clone)]
enum Expr {
	Value(Num),
	Computation(Computation),
}

#[derive(Debug, Clone)]
struct Monkey {
	name: String,
	expr: Expr,
}

impl FromStr for Monkey {
	type Err = String;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let re = regex!(
			r#"(?P<name>\w+): (?:(?P<value>\d+)|(?:(?P<m1>\w+) (?P<op>[-+*/]) (?P<m2>\w+)))"#
		);

		let m = re.captures(s).to_result()?;

		let expr = if let Some(digits) = m.name("value") {
			let n = digits.as_str().parse::<Num>().map_err(|e| e.to_string())?;
			Expr::Value(n)
		} else {
			let [m1, op, m2] = ["m1", "op", "m2"].try_map(|cap| m.name(cap).to_result())?;
			let [m1, m2] = [m1, m2].map(|m| m.as_str().to_owned());
			let op = op.as_str().parse()?;
			Expr::Computation(Computation {
				operator: op,
				a: m1,
				b: m2,
			})
		};

		let name = m.name("name").to_result()?.as_str().to_owned();

		Ok(Monkey { name, expr })
	}
}

fn read_input(input: &str) -> Result<Vec<Monkey>, String> {
	input.lines().map(Monkey::from_str).collect()
}

// end::setup[]

// tag::pt1[]
fn pt1(monkeys: Vec<Monkey>) -> Ans {
	fn resolve(name: &str, map: &HashMap<String, Expr>) -> Ans {
		match &map[name] {
			&Expr::Value(n) => n,
			Expr::Computation(Computation { operator, a, b }) => {
				let x = resolve(a, map);
				let y = resolve(b, map);
				operator.apply(x, y)
			}
		}
	}

	let monkey_map = monkeys
		.into_iter()
		.map(|Monkey { name, expr }| (name, expr))
		.collect::<HashMap<_, _>>();

	resolve("root", &monkey_map)
}
// end::pt1[]

// tag::pt2[]
fn pt2(monkeys: Vec<Monkey>) -> Ans {
	#[derive(Debug, Clone)]
	enum Direction {
		ValueThenSubtree,
		SubtreeThenValue,
	}
	#[derive(Debug, Clone)]
	struct Tree {
		operator: Operator,
		value: Num,
		subtree: Box<Node>,
		direction: Direction,
	}

	#[derive(Debug, Clone)]
	enum Node {
		Unknown,
		Value(Num),
		Tree(Tree),
	}

	#[derive(Debug, Clone)]
	struct Root {
		node: Node,
		value: Num,
	}

	fn get_root(map: &HashMap<String, Expr>) -> Root {
		match &map["root"] {
			Expr::Computation(Computation { operator: _, a, b }) => {
				let left = get_node(a, map);
				let right = get_node(b, map);

				let (node, value) = match (left, right) {
					(Node::Value(value), node) | (node, Node::Value(value)) => (node, value),
					_ => panic!("logic error: did not get one tree and one value from root"),
				};
				Root { node, value }
			}
			Expr::Value(_) => panic!("root was a literal number"),
		}
	}

	fn get_node(name: &str, map: &HashMap<String, Expr>) -> Node {
		use Direction::*;
		if name == "humn" {
			Node::Unknown
		} else {
			match &map[name] {
				&Expr::Value(n) => Node::Value(n),
				Expr::Computation(Computation { operator, a, b }) => {
					let left = get_node(a, map);
					let right = get_node(b, map);
					match (left, right) {
						(Node::Value(v1), Node::Value(v2)) => Node::Value(operator.apply(v1, v2)),
						(Node::Value(value), node) => Node::Tree(Tree {
							operator: *operator,
							value,
							subtree: node.into(),
							direction: ValueThenSubtree,
						}),
						(node, Node::Value(value)) => Node::Tree(Tree {
							operator: *operator,
							value,
							subtree: node.into(),
							direction: SubtreeThenValue,
						}),
						_ => panic!("logic error: did not get one tree and one value from root"),
					}
				}
			}
		}
	}

	fn solve_eqn(
		Root {
			mut node,
			value: mut x,
		}: Root,
	) -> Ans {
		use Direction::*;
		use Operator::*;
		loop {
			match node {
				Node::Unknown => return x,
				Node::Tree(Tree {
					operator,
					value,
					subtree,
					direction,
				}) => {
					match operator {
						Add => x -= value,
						Mul => x /= value,
						Sub => match direction {
							SubtreeThenValue => x += value,
							ValueThenSubtree => x = value - x,
						},
						Div => match direction {
							SubtreeThenValue => x *= value,
							ValueThenSubtree => x = value / x,
						},
					}

					node = Box::into_inner(subtree);
				}
				Node::Value(_) => panic!("logic error"),
			}
		}
	}

	let monkey_map = monkeys
		.into_iter()
		.map(|Monkey { name, expr }| (name, expr))
		.collect::<HashMap<_, _>>();

	solve_eqn(get_root(&monkey_map))
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
			(pt1, 152),
			(pt2, 301),
		);
		run_tests(
			read_input(&read_file!("input.txt")).unwrap(),
			(pt1, 84_244_467_642_604),
			(pt2, 3_759_569_926_192),
		);
	}
}
