// tag::setup[]
use crate::{Answer, AocResult, ToResultDefaultErr};

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	(2, (pt1(input), pt2(input))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

#[derive(Clone, Copy, Debug)]
enum Interpretation {
	Pt1,
	Pt2,
}

#[derive(Clone, Copy, Debug)]
enum RpsChoice {
	Rock,
	Paper,
	Scissors,
}

struct Round {
	me: RpsChoice,
	opponent: RpsChoice,
}

impl Round {
	fn new(opponent_str: &str, me_str: &str, intepretation: Interpretation) -> AocResult<Self> {
		use Interpretation::*;
		use RpsChoice::*;

		let opponent = match opponent_str {
			"A" => Rock,
			"B" => Paper,
			"C" => Scissors,
			_ => return AocResult::Err(format!("Invalid choice {opponent_str:?}")),
		};

		let me = match intepretation {
			Pt1 => match me_str {
				"X" => Rock,
				"Y" => Paper,
				"Z" => Scissors,
				_ => return AocResult::Err(format!("Invalid choice {me_str:?}")),
			},
			Pt2 => match me_str {
				"X" => match opponent {
					Rock => Scissors,
					Paper => Rock,
					Scissors => Paper,
				},
				"Y" => opponent,
				"Z" => match opponent {
					Rock => Paper,
					Paper => Scissors,
					Scissors => Rock,
				},
				_ => return AocResult::Err(format!("Invalid choice {me_str:?}")),
			},
		};

		Ok(Self { me, opponent })
	}

	fn value(self) -> usize {
		use RpsChoice::*;

		let Self { opponent, me } = self;
		let choice_score = match me {
			Rock => 1,
			Paper => 2,
			Scissors => 3,
		};
		let game_score = match (me, opponent) {
			(Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => 6, // win
			(Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 0, // loss
			(Rock, Rock) | (Scissors, Scissors) | (Paper, Paper) => 3, // tie
		};
		choice_score + game_score
	}
}

fn read_input(input: &str, interpretation: Interpretation) -> AocResult<Vec<Round>> {
	let mut rounds = Vec::new();
	for line in input.lines() {
		if line.is_empty() {
			continue;
		}
		let mut comps = line.split_ascii_whitespace();
		rounds.push(Round::new(
			comps.next().to_result()?,
			comps.next().to_result()?,
			interpretation,
		)?);
	}
	Ok(rounds)
}
// end::setup[]

// tag::pt1[]
fn pt1(input: &str) -> usize {
	let rounds = read_input(input, Interpretation::Pt1).unwrap();
	rounds.into_iter().map(Round::value).sum()
}
// end::pt1[]

// tag::pt2[]
fn pt2(input: &str) -> usize {
	let rounds = read_input(input, Interpretation::Pt2).unwrap();
	rounds.into_iter().map(Round::value).sum()
}
// end::pt2[]

#[cfg(test)]
mod test {
	#![allow(unused_imports)]
	use super::*;
	use crate::{run_test, TestCase};

	#[test]
	fn test() {
		run_test(include_str!("sample_input.txt"), ((pt1, 15), (pt2, 12)));
		run_test(include_str!("input.txt"), ((pt1, 13682), (pt2, 12881)));
	}
}
