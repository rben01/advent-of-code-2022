// tag::setup[]
use crate::Answer;
use ndarray::{prelude::*, NdProducer, SliceInfo, SliceInfoElem, Zip as ArrayZip};

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let mat = read_input(input).expect("couldn't read input");
	(8, (pt1(mat.view()), pt2(mat.view()))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

type Height = i32;
type Matrix = Array2<Height>;
type MatrixView<'a> = ArrayView2<'a, Height>;

fn read_input(input: &str) -> Option<Matrix> {
	let mut heights_vec = Vec::with_capacity(input.len());
	let mut mat_width = 0_usize;
	let mut mat_height = 0_usize;

	for line in input.lines() {
		mat_width = line.len();
		mat_height += 1;

		for c in line.chars() {
			let n = c
				.to_digit(10)
				.and_then(|d| Height::try_from(d).ok())
				.unwrap_or_else(|| panic!("could not convert \"{c}\" to a digit"));
			heights_vec.push(n);
		}
	}

	let mat = Matrix::from_shape_vec((mat_width, mat_height), heights_vec).ok()?;
	Some(mat)
}
// end::setup[]

// tag::pt1[]
fn pt1(forest: MatrixView) -> usize {
	fn find_visible_trees<'a>(
		forest_lanes: impl NdProducer<Item = ArrayView1<'a, Height>, Dim = Ix1>,
		visibility_mask_lanes: impl NdProducer<Item = ArrayViewMut1<'a, bool>, Dim = Ix1>,
		direction: SliceInfo<[SliceInfoElem; 1], Ix1, Ix1>,
	) {
		ArrayZip::from(forest_lanes)
			.and(visibility_mask_lanes)
			.for_each(|tree_lane, mut mask_lane| {
				// To count 0s on edges as visible
				let mut max_height = -1;
				ArrayZip::from(tree_lane.slice(direction))
					.and(mask_lane.slice_mut(direction))
					.for_each(|&tree, visible| {
						if tree > max_height {
							*visible = true;
							max_height = tree;
						}
					});
			});
	}

	let mut visibility_mask = Array2::from_elem(forest.dim(), false);

	let forward = s![..];
	let backward = s![..;-1];

	find_visible_trees(forest.rows(), visibility_mask.rows_mut(), forward);
	find_visible_trees(forest.rows(), visibility_mask.rows_mut(), backward);
	find_visible_trees(forest.columns(), visibility_mask.columns_mut(), forward);
	find_visible_trees(forest.columns(), visibility_mask.columns_mut(), backward);

	visibility_mask.into_iter().filter(|&b| b).count()
}
// end::pt1[]

// tag::pt2[]
fn pt2(forest: MatrixView) -> usize {
	fn n_trees_visible(
		forest: MatrixView,
		candidate: Height,
		index_iter: impl IntoIterator<Item = usize>,
		forest_indexer: impl Fn(usize) -> (usize, usize),
	) -> usize {
		let mut count = 0;
		for i in index_iter {
			count += 1;

			let forest_index = forest_indexer(i);
			let tree = forest[forest_index];
			if tree >= candidate {
				break;
			}
		}
		count
	}

	let mut best_score = usize::MIN;
	let (height, width) = forest.dim();
	for r in 0..height {
		for c in 0..width {
			let candidate = forest[(r, c)];

			let above = n_trees_visible(forest, candidate, (0..r).rev(), |i| (i, c));
			let below = n_trees_visible(forest, candidate, (r + 1)..height, |i| (i, c));
			let to_left = n_trees_visible(forest, candidate, (0..c).rev(), |i| (r, i));
			let to_right = n_trees_visible(forest, candidate, (c + 1)..width, |i| (r, i));
			let score = above * below * to_left * to_right;

			if score > best_score {
				best_score = score;
			}
		}
	}

	best_score
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
			read_input(include_str!("sample_input.txt")).unwrap().view(),
			(pt1, 21),
			(pt2, 8),
		);
		run_tests(
			read_input(include_str!("input.txt")).unwrap().view(),
			(pt1, 1733),
			(pt2, 284_648),
		);
	}
}
