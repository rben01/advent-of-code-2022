#![feature(binary_heap_into_iter_sorted)]
#![feature(iter_array_chunks)]
#![feature(iterator_try_collect)]
#![warn(clippy::pedantic)]
#![allow(
	clippy::enum_glob_use,
	clippy::missing_panics_doc,
	clippy::must_use_candidate,
	clippy::redundant_closure_for_method_calls,
	clippy::type_complexity
)]

use std::fmt::{Debug, Display};

pub type AocResult<T> = Result<T, String>;
pub trait ToResultDefaultErr<T> {
	/// Converts something to a "default" Result
	/// # Ok
	/// Good values are preserved, turning into `Ok(x)`
	/// # Errors
	/// Errors are turned into `Err(default error message)` where the error message is
	/// determined by the trait implementation (by the time you're at the call site, the
	/// error message is already set in stone)
	fn to_result(self) -> AocResult<T>;
}

impl<T> ToResultDefaultErr<T> for Option<T> {
	fn to_result(self) -> AocResult<T> {
		match self {
			Some(t) => Ok(t),
			None => Err(format!(
				"expected a Some({:?}) but got None",
				std::any::type_name::<T>(),
			)),
		}
	}
}

// tag::mods[]
macro_rules! include_days {
	($($mod_name:ident:$ft_name:literal),* $(,)?) => {
		$(#[cfg(feature = $ft_name)] pub mod $mod_name;)*
	};
}

include_days!(
	day_01:"day_01",
	day_02:"day_02",
	day_03:"day_03",
	day_04:"day_04",
	day_05:"day_05",
	day_06:"day_06",
	day_07:"day_07",
	day_08:"day_08",
	day_09:"day_09",
	day_10:"day_10",
	day_11:"day_11",
	day_12:"day_12",
	day_13:"day_13",
	day_14:"day_14",
	day_15:"day_15",
	day_16:"day_16",
	day_17:"day_17",
	day_18:"day_18",
	day_19:"day_19",
	day_20:"day_20",
	day_21:"day_21",
	day_22:"day_22",
	day_23:"day_23",
	day_24:"day_24",
	day_25:"day_25",
);

// end::mods[]
#[derive(Debug, PartialEq, Eq)]
pub struct Answer<T1, T2> {
	day: usize,
	pt1: T1,
	pt2: T2,
}

impl<T1: Debug, T2: Debug> Display for Answer<T1, T2> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let Answer { day, pt1, pt2 } = self;
		write!(f, "Day: {day} ; Part 1: {pt1:?} ; Part 2: {pt2:?}")
	}
}

impl<T1, T2> From<(usize, (T1, T2))> for Answer<T1, T2> {
	fn from((day, (pt1, pt2)): (usize, (T1, T2))) -> Self {
		Self { day, pt1, pt2 }
	}
}

#[cfg(test)]
#[track_caller]
pub(crate) fn run_test<Input, AnsFunc, Actual, Expected>(
	input: Input,
	test_case: (AnsFunc, Expected),
) where
	AnsFunc: Fn(Input) -> Actual,
	Actual: Eq + Debug,
	Expected: Eq + Debug + PartialEq<Actual>,
{
	let (get_ans, expected) = test_case;
	let actual = get_ans(input);
	assert_eq!(expected, actual);
}

#[cfg(test)]
#[track_caller]
pub(crate) fn run_tests<Input, AnsFunc1, AnsFunc2, Actual1, Actual2, Expected1, Expected2>(
	input: Input,
	test_case_1: (AnsFunc1, Expected1),
	test_case_2: (AnsFunc2, Expected2),
) where
	AnsFunc1: Fn(Input) -> Actual1,
	AnsFunc2: Fn(Input) -> Actual2,
	Actual1: Eq + Debug,
	Actual2: Eq + Debug,
	Expected1: Eq + Debug + PartialEq<Actual1>,
	Expected2: Eq + Debug + PartialEq<Actual2>,
	Input: Copy,
{
	run_test(input, test_case_1);
	run_test(input, test_case_2);
}
