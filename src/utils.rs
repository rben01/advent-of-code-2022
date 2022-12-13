#![allow(dead_code)]
use std::{cell::RefCell, rc::Rc};

pub type RcRc<T> = Rc<RefCell<T>>;

pub fn into_rc_rc<T>(x: T) -> RcRc<T> {
	Rc::new(RefCell::new(x))
}

pub fn get_nsew_adjacent(
	pos: (usize, usize),
	row_bounds: (usize, usize),
	col_bounds: (usize, usize),
) -> [Option<(usize, usize)>; 4] {
	enum D {
		MinusOne,
		Zero,
		PlusOne,
	}
	use D::*;

	let (row, col) = pos;
	let (min_row, max_row) = row_bounds;
	let (min_col, max_col) = col_bounds;

	[
		(MinusOne, Zero),
		(PlusOne, Zero),
		(Zero, MinusOne),
		(Zero, PlusOne),
	]
	.map(|(dr, dc)| {
		let new_row = match dr {
			MinusOne => row.checked_sub(1)?,
			Zero => row,
			PlusOne => row + 1,
		};
		let new_col = match dc {
			MinusOne => col.checked_sub(1)?,
			Zero => col,
			PlusOne => col + 1,
		};

		((min_row..=max_row).contains(&new_row) && (min_col..=max_col).contains(&new_col))
			.then_some((new_row, new_col))
	})
}
