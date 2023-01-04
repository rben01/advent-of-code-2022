#![feature(binary_heap_into_iter_sorted)]
#![feature(iter_array_chunks)]
#![feature(iterator_try_collect)]
#![feature(array_try_map)]
#![feature(array_zip)]
#![warn(clippy::pedantic)]
#![allow(
	clippy::too_many_lines,
	clippy::enum_glob_use,
	clippy::missing_panics_doc,
	clippy::must_use_candidate,
	clippy::redundant_closure_for_method_calls,
	clippy::type_complexity
)]

mod utils;

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
		write!(f, "Day {day:0>2}: {:?}", (pt1, pt2))
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
	Actual: Debug,
	Expected: Debug + PartialEq<Actual>,
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
	Actual1: Debug,
	Actual2: Debug,
	Expected1: Debug + PartialEq<Actual1>,
	Expected2: Debug + PartialEq<Actual2>,
	Input: Clone,
{
	run_test(input.clone(), test_case_1);
	run_test(input, test_case_2);
}

#[macro_export]
macro_rules! regex {
	($re:expr $(,)?) => {{
		static RE: once_cell::sync::OnceCell<regex::Regex> = once_cell::sync::OnceCell::new();
		RE.get_or_init(|| regex::Regex::new($re).unwrap())
	}};
}

#[macro_export]
macro_rules! read_file {
	($filename:literal) => {{
		use std::path::{Path, PathBuf};
		// `file!()` returns the path where `read_file!` is invoked, not the file where
		// it's defined (this very file)
		let file_relpath = Path::new(file!());

		let child_path = [
			Path::new(env!("CARGO_MANIFEST_DIR")),
			file_relpath.parent().unwrap(),
			Path::new($filename),
		]
		.into_iter()
		.collect::<PathBuf>();

		std::fs::read_to_string(&child_path)
			.unwrap_or_else(|_| panic!("could not read file {child_path:?}"))
	}};
}

pub trait Cast {
	fn cast<U>(self) -> U
	where
		Self: Copy + Debug + TryInto<U>,
		<Self as TryInto<U>>::Error: Debug;
}

impl<T> Cast for T {
	fn cast<U>(self) -> U
	where
		T: Copy + Debug + TryInto<U>,
		<T as TryInto<U>>::Error: Debug,
	{
		self.try_into().unwrap_or_else(|e| {
			panic!(
				"could not convert {:?} to {:?} (original error: {:?})",
				self,
				std::any::type_name::<U>(),
				e
			)
		})
	}
}
