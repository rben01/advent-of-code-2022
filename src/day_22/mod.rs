// tag::setup[]
use crate::{read_file, utils::get_nsew_adjacent, Answer, Cast};
use itertools::iproduct;
use ndarray::prelude::{Array2, ArrayView2};
use std::{collections::HashMap, ops::Index};
use strum::EnumCount;
use strum_macros::EnumCount;

type Coord = i32;
type Ans = i32;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let nums = read_input(input).expect("could not read input");
	(22, (pt1(&nums), pt2(&nums))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("sample_input.txt"))
}

#[derive(Debug, Clone, Copy)]
enum Heading {
	N,
	S,
	E,
	W,
}

impl Heading {
	fn value(self) -> Ans {
		use Heading::*;
		match self {
			E => 0,
			S => 1,
			W => 2,
			N => 3,
		}
	}

	fn turned_left(self) -> Self {
		use Heading::*;
		match self {
			N => W,
			W => S,
			S => E,
			E => N,
		}
	}

	fn turned_right(self) -> Self {
		use Heading::*;
		match self {
			N => E,
			E => S,
			S => W,
			W => N,
		}
	}
}

#[derive(Debug, Clone, Copy)]
enum Tile {
	Aether,
	Open,
	Solid,
}

impl TryFrom<char> for Tile {
	type Error = String;

	fn try_from(value: char) -> Result<Self, Self::Error> {
		use Tile::*;
		let tile = match value {
			' ' => Aether,
			'.' => Open,
			'#' => Solid,
			_ => return Err("could not parse {value:?} into a Tile".into()),
		};
		Ok(tile)
	}
}

#[derive(Debug, Clone, Copy)]
struct Range {
	lo: Coord,
	hi: Coord,
}

impl Range {
	fn size(self) -> Coord {
		self.hi - self.lo + 1
	}
}

#[derive(Debug, Clone, Copy)]
enum Step {
	Forward(Coord),
	TurnLeft,
	TurnRight,
}

#[derive(Debug, Clone)]
struct Map {
	grid: Array2<Tile>,
	row_ranges: Vec<Range>,
	col_ranges: Vec<Range>,
	steps: Vec<Step>,
}

fn read_input(input: &str) -> Result<Map, String> {
	let (n_rows, n_cols) = input
		.lines()
		.map(|line| line.trim_end().len())
		.take_while(|&len| len > 0)
		.fold((0_usize, usize::MIN), |(nr, nc), len| (nr + 1, nc.max(len)));

	let mut tiles = Vec::with_capacity(input.len());
	let mut row_ranges = vec![
		Range {
			lo: (n_cols - 1).cast(),
			hi: 0
		};
		n_rows
	];
	let mut col_ranges = vec![
		Range {
			lo: (n_rows - 1).cast(),
			hi: 0
		};
		n_cols
	];

	let mut lines = input.lines();
	for (row_idx, line) in lines.by_ref().enumerate() {
		if line.trim().is_empty() {
			break;
		}

		for (col_idx, c) in line
			.chars()
			.chain(std::iter::repeat(' '))
			.enumerate()
			.take(n_cols)
		{
			let tile = c.try_into()?;
			tiles.push(tile);

			if !matches!(tile, Tile::Aether) {
				let row_range = &mut row_ranges[row_idx];
				row_range.lo = row_range.lo.min(col_idx.cast());
				row_range.hi = row_range.hi.max(col_idx.cast());

				let col_range = &mut col_ranges[col_idx];
				col_range.lo = col_range.lo.min(row_idx.cast());
				col_range.hi = col_range.hi.max(row_idx.cast());
			}
		}
	}

	let grid = Array2::from_shape_vec((n_rows, n_cols), tiles).map_err(|e| e.to_string())?;

	let line = lines
		.next()
		.ok_or_else(|| "no steps line found".to_owned())?;
	let mut steps = Vec::new();
	let mut num_steps = 0;
	for c in line.chars() {
		match c {
			'L' | 'R' => {
				steps.push(Step::Forward(num_steps));
				num_steps = 0;
				let turn = if c == 'L' {
					Step::TurnLeft
				} else {
					Step::TurnRight
				};
				steps.push(turn);
			}
			_ => {
				let n = c
					.to_digit(10)
					.ok_or_else(|| "could not convert {c:?} to digit".to_owned())?;
				num_steps = num_steps * 10 + n.cast::<Coord>();
			}
		}
	}
	steps.push(Step::Forward(num_steps));

	Ok(Map {
		grid,
		row_ranges,
		col_ranges,
		steps,
	})
}

// end::setup[]

// tag::pt1[]
fn pt1(map: &Map) -> Ans {
	use Heading::*;

	let Map {
		grid,
		row_ranges,
		col_ranges,
		steps,
	} = map;

	let mut row: Coord = 0;
	let mut col = grid
		.row(0)
		.iter()
		.position(|tile| matches!(tile, Tile::Open))
		.expect("no open tile found in first row")
		.cast::<Coord>();
	let mut heading = Heading::E;

	'this_step: for step in steps {
		match step {
			&Step::Forward(n_steps) => match heading {
				N | S => {
					let range = col_ranges[col.cast::<usize>()];
					// Maximum number of steps we can take before we return to our starting
					// position
					let n_steps_before_loop = n_steps.min(range.size());
					// Number of steps remaining after looping (0 if n_steps wasn't big
					// enough to loop)
					let n_steps_after_loop = n_steps - n_steps_before_loop;

					let sign = if matches!(heading, N) { -1 } else { 1 };

					let lo = range.lo;
					for _ in 0..n_steps_before_loop {
						let next_r = lo + (row - lo + sign).rem_euclid(range.size());
						if matches!(grid[[next_r.cast(), col.cast()]], Tile::Solid) {
							continue 'this_step;
						}
						row = next_r;
					}

					// If we're here, we haven't hit a closed tile yet
					row = (lo + (row - lo + sign * n_steps_after_loop).rem_euclid(range.size()))
						.cast();
				}
				E | W => {
					let range = row_ranges[row.cast::<usize>()];
					let n_steps_before_loop = n_steps.min(range.size());
					let n_steps_after_loop = n_steps - n_steps_before_loop;

					let sign = if matches!(heading, W) { -1 } else { 1 };

					let lo = range.lo;
					for _ in 0..n_steps_before_loop {
						let next_c = lo + (col - lo + sign).rem_euclid(range.size());
						if matches!(grid[[row.cast(), next_c.cast()]], Tile::Solid) {
							continue 'this_step;
						}
						col = next_c;
					}

					// If we're here, we haven't hit a closed tile yet
					col = (lo + (col - lo + sign * n_steps_after_loop).rem_euclid(range.size()))
						.cast();
				}
			},
			Step::TurnLeft => heading = heading.turned_left(),
			Step::TurnRight => heading = heading.turned_right(),
		}
	}

	1000 * (row + 1) + 4 * (col + 1) + heading.value()
}
// end::pt1[]

// tag::pt2[]
/// A direction, positive (`p`) or negative (`n`), along an axis (`X`, `Y`, or `Z`).
/// E.g., `Xp` represents pointing along the positive x-axis, `Zn` along the negative
/// z-axis, etc.
#[derive(Debug, Clone, Copy, EnumCount, PartialEq, Eq, Hash)]
#[repr(usize)]
enum Direction {
	/// The -x direction
	Xn,
	/// The +x direction
	Xp,
	/// The -y direction
	Yn,
	/// The +y direction
	Yp,
	/// The -z direction
	Zn,
	/// The +z direction
	Zp,
}

#[derive(Debug, Clone, Copy)]
enum RevolutionMovingZp {
	Identity,
	ZpToXn,
	ZpToXp,
	ZpToYn,
	ZpToYp,
	ZpToZnAboutX,
}

impl RevolutionMovingZp {
	fn inverted(self) -> Self {
		use RevolutionMovingZp::*;
		match self {
			Identity => Identity,
			ZpToXn => ZpToXp,
			ZpToXp => ZpToXn,
			ZpToYn => ZpToYp,
			ZpToYp => ZpToYn,
			ZpToZnAboutX => ZpToZnAboutX,
		}
	}

	/// Apply this revolution to the given direction
	fn apply(self, dir: Direction) -> Direction {
		use Direction::*;
		use RevolutionMovingZp::*;

		match (dir, self) {
			(_, Identity) => dir,
			(Xn, ZpToYn | ZpToYp | ZpToZnAboutX) | (Zn, ZpToXp) | (Zp, ZpToXn) => Xn,
			(Xp, ZpToYn | ZpToYp | ZpToZnAboutX) | (Zn, ZpToXn) | (Zp, ZpToXp) => Xp,
			(Yn, ZpToXn | ZpToXp) | (Yp, ZpToZnAboutX) | (Zn, ZpToYp) | (Zp, ZpToYn) => Yn,
			(Yn, ZpToZnAboutX) | (Yp, ZpToXn | ZpToXp) | (Zn, ZpToYn) | (Zp, ZpToYp) => Yp,
			(Xn, ZpToXn) | (Xp, ZpToXp) | (Yn, ZpToYn) | (Yp, ZpToYp) | (Zp, ZpToZnAboutX) => Zn,
			(Xn, ZpToXp) | (Xp, ZpToXn) | (Yn, ZpToYp) | (Yp, ZpToYn) | (Zn, ZpToZnAboutX) => Zp,
		}
	}
}

#[derive(Debug, Clone, Copy)]
enum RotationAboutZp {
	Zero,
	OneQuarterCcw,
	HalfTurn,
	ThreeQuartersCcw,
}

impl RotationAboutZp {
	fn inverted(self) -> Self {
		use RotationAboutZp::*;
		match self {
			Zero => Zero,
			OneQuarterCcw => ThreeQuartersCcw,
			HalfTurn => HalfTurn,
			ThreeQuartersCcw => OneQuarterCcw,
		}
	}

	/// Apply this rotation to the given direction
	fn apply(self, dir: Direction) -> Direction {
		use Direction::*;
		use RotationAboutZp::*;

		match (dir, self) {
			(_, Zero) => dir,
			(Xp, HalfTurn) | (Yn, OneQuarterCcw) | (Yp, ThreeQuartersCcw) => Xn,
			(Xn, HalfTurn) | (Yn, ThreeQuartersCcw) | (Yp, OneQuarterCcw) => Xp,
			(Xn, ThreeQuartersCcw) | (Xp, OneQuarterCcw) | (Yp, HalfTurn) => Yn,
			(Xn, OneQuarterCcw) | (Xp, ThreeQuartersCcw) | (Yn, HalfTurn) => Yp,
			(Zn, _) => Zn,
			(Zp, _) => Zp,
		}
	}
}

#[derive(Debug, Clone, Copy)]
struct Transformation {
	revolution: RevolutionMovingZp,
	rotation: RotationAboutZp,
}

impl Transformation {
	fn apply(self, dir: Direction) -> Direction {
		let Self {
			revolution,
			rotation,
		} = self;

		rotation.apply(revolution.apply(dir))
	}

	fn apply_inverse(self, dir: Direction) -> Direction {
		let Self {
			revolution,
			rotation,
		} = self;
		rotation.inverted().apply(revolution.inverted().apply(dir))
	}
}

/// An oriented face of the cube; it's on one side of the cube, and its compass's North
/// is pointing in a known direction
#[derive(Debug, Clone, Copy)]
struct CubeFace {
	side: Direction,
	north: Direction,
}

impl CubeFace {
	fn from_adjacent_face(side: Direction, north: Direction, prev_face_adj_edge: Heading) -> Self {
		use Direction::*;
		use Heading::*;

		let (my_side, my_north) = match (side, north, prev_face_adj_edge) {
			(Yn, Xp, S) | (Yp, Xn, N) | (Zn, Yn, W) | (Zp, Yn, E) => (Xn, Yn),
			(Yn, Xn, N) | (Yp, Xp, S) | (Zn, Yp, E) | (Zp, Yp, W) => (Xn, Yp),
			(Yn, Zp, W) | (Yp, Zp, E) | (Zn, Xn, N) | (Zp, Xp, S) => (Xn, Zp),
			(Yn, Zn, E) | (Yp, Zn, W) | (Zn, Xp, S) | (Zp, Xn, N) => (Xn, Zn),

			(Yn, Xn, S) | (Yp, Xp, N) | (Zn, Yn, E) | (Zp, Yn, W) => (Xp, Yn),
			(Yn, Xp, N) | (Yp, Xn, S) | (Zn, Yp, W) | (Zp, Yp, E) => (Xp, Yp),
			(Yn, Zn, W) | (Yp, Zn, E) | (Zn, Xn, S) | (Zp, Xp, N) => (Xp, Zn),
			(Yn, Zp, E) | (Yp, Zp, W) | (Zn, Xp, N) | (Zp, Xn, S) => (Xp, Zp),

			(Xn, Yp, S) | (Xp, Yn, N) | (Zn, Xn, E) | (Zp, Xn, W) => (Yn, Xn),
			(Xn, Yn, N) | (Xp, Yp, S) | (Zn, Xp, W) | (Zp, Xp, E) => (Yn, Xp),
			(Xn, Zn, W) | (Xp, Zn, E) | (Zn, Yp, S) | (Zp, Yn, N) => (Yn, Zn),
			(Xn, Zp, E) | (Xp, Zp, W) | (Zn, Yn, N) | (Zp, Yp, S) => (Yn, Zp),

			(Xn, Yn, S) | (Xp, Yp, N) | (Zn, Xn, W) | (Zp, Xn, E) => (Yp, Xn),
			(Xn, Yp, N) | (Xp, Yn, S) | (Zn, Xp, E) | (Zp, Xp, W) => (Yp, Xp),
			(Xn, Zn, E) | (Xp, Zn, W) | (Zn, Yn, S) | (Zp, Yp, N) => (Yp, Zn),
			(Xn, Zp, W) | (Xp, Zp, E) | (Zn, Yp, N) | (Zp, Yn, S) => (Yp, Zp),

			(Xn, Zp, S) | (Xp, Zn, N) | (Yn, Xn, W) | (Yp, Xn, E) => (Zn, Xn),
			(Xn, Zn, N) | (Xp, Zp, S) | (Yn, Xp, E) | (Yp, Xp, W) => (Zn, Xp),
			(Xn, Yn, E) | (Xp, Yn, W) | (Yn, Zp, S) | (Yp, Zn, N) => (Zn, Yn),
			(Xn, Yp, W) | (Xp, Yp, E) | (Yn, Zn, N) | (Yp, Zp, S) => (Zn, Yp),

			(Xn, Zn, S) | (Xp, Zp, N) | (Yn, Xn, E) | (Yp, Xn, W) => (Zp, Xn),
			(Xn, Zp, N) | (Xp, Zn, S) | (Yn, Xp, W) | (Yp, Xp, E) => (Zp, Xp),
			(Xn, Yn, W) | (Xp, Yn, E) | (Yn, Zn, S) | (Yp, Zp, N) => (Zp, Yn),
			(Xn, Yp, E) | (Xp, Yp, W) | (Yn, Zp, N) | (Yp, Zn, S) => (Zp, Yp),

			(Xn | Xp, Xn | Xp, _) | (Yn | Yp, Yn | Yp, _) | (Zn | Zp, Zn | Zp, _) => {
				panic!("invalid combination of (prev) face and north: {side:?}, {north:?}")
			}
		};

		Self {
			side: my_side,
			north: my_north,
		}
	}

	fn apply(self, transformation: Transformation) -> Self {
		let Self { side, north } = self;
		let [side, north] = [side, north].map(|dir| transformation.apply(dir));
		Self { side, north }
	}

	fn apply_inverse(self, transformation: Transformation) -> Self {
		let Self { side, north } = self;
		let [side, north] = [side, north].map(|dir| transformation.apply_inverse(dir));
		Self { side, north }
	}

	/// Get the transformation that gets `self`'s `side` to be `Zp` and its `north` to be
	/// `Yp`
	fn get_centralizing_transform(self) -> Transformation {
		use Direction::*;
		use RevolutionMovingZp::*;
		use RotationAboutZp::*;

		let Self { side, north } = self;

		// revolve side to get it to Zp
		let revolution = match side {
			Xn => ZpToXp,
			Xp => ZpToXn,
			Yn => ZpToYp,
			Yp => ZpToYn,
			Zn => ZpToZnAboutX,
			Zp => Identity,
		};

		// rotate the revoled north to get it to point to Yp
		let rotation = match revolution.apply(north) {
			Xn => OneQuarterCcw,
			Xp => ThreeQuartersCcw,
			Yn => HalfTurn,
			Yp => Zero,
			Zn | Zp => panic!(
				"after revolution to get self to Zp, self was still pointing toward {north:?}"
			),
		};

		Transformation {
			revolution,
			rotation,
		}
	}

	/// Get the face on the cube adjacent to face `face_idx`'s `heading` edge
	fn get_side_of_adjacent_face(self, at_heading: Heading) -> Direction {
		use Direction::*;
		use Heading::*;

		// The side that would be at `at_heading` if we were centralized
		let side_at_heading = match at_heading {
			N => Yp,
			S => Yn,
			E => Xp,
			W => Xn,
		};

		// But we're not centralized, so apply the inverse transform to get back to
		// reality

		self.get_centralizing_transform()
			.apply_inverse(side_at_heading)
	}

	// fn get_relative_heading_of_adjacent_face(
	// 	self,
	// 	at_heading: Heading,
	// 	adj_face_north: Direction,
	// ) -> Heading {
	// 	use Direction::*;
	// 	use Heading::*;

	// 	let Self { side, north } = self;
	// 	let adj_face_side = self.get_side_of_adjacent_face(at_heading);
	// 	// let relative_north = match (side, adj_face_side, (north, adj_face_north)) {
	// 	// 	(Xn, Yn, ()) => N,
	// 	// 	(Xn | Xp, Xn | Xp, _) | (Yn | Yp, Yn | Yp, _) | (Zn | Zp, Zn | Zp, _) => {
	// 	// 		panic!("adjacent face was not actually adjacent")
	// 	// 	}
	// 	// };

	// 	todo!()
	// }
}

// #[derive(Debug, Clone, Copy)]
// struct AbstractCube {
// 	/// The side length of the cube — the faces in the grid will be `side_len * side_len`
// 	/// tiles
// 	side_len: usize,
// 	/// The faces of the cube; `faces[i]` is a pair...
// 	/// 1. Whose first element is the side and North direction of face `i` (the
// 	/// assignment of indices to faces is more or less arbitrary), and...
// 	/// 2. Whose second element is the location in the grid (`[row, col]`) of face `i`,
// 	/// *after* the grid has been divvied up into sub-areas of size `side_len *
// 	/// side_len`. So the cube's bottommost face in the grid might have a `row`-index of,
// 	/// say, 3 or 4, and not `3 * side_len` or `4 * side_len`.
// 	faces: [CubeFace; Direction::COUNT],
// 	/// If `direction: Direction`, then `face_indices[direction as usize]` is the index
// 	/// (as would be used to index `faces`) of the `direction` face of the cube
// 	face_indices: [usize; Direction::COUNT],
// }

#[derive(Debug, Clone, Copy)]
struct Cube<'a> {
	/// The side length of the cube — the faces in the grid will be `side_len * side_len`
	/// tiles
	side_len: usize,
	/// The original grid of tiles that the faces of this cube were laid flat on by
	/// unfolding the cube
	grid: ArrayView2<'a, Tile>,
	/// `grid_faces[i]` is the location in the grid (`[row, col]`) of face `i`, *after*
	/// the grid has been divvied up into sub-areas of size `side_len * side_len`. So the
	/// cube's bottommost face in the grid might have a `row`-index of, say, 3 or 4, and
	/// not `3 * side_len` or `4 * side_len`.
	face_locs_in_grid: [[Coord; 2]; Direction::COUNT],
	/// `faces[i]` is the side and North direction of face `i` (the assignment of indices
	/// to faces is more or less arbitrary)
	faces: [CubeFace; Direction::COUNT],
}

// impl Index<usize> for Cube<'_> {
// 	type Output = CubeFace;

// 	fn index(&self, index: usize) -> &Self::Output {
// 		&self.faces[index]
// 	}
// }

// impl Index<Direction> for Cube<'_> {
// 	type Output = (usize, CubeFace);

// 	fn index(&self, index: Direction) -> &Self::Output {
// 		let idx = self.face_indices[index as usize];
// 		&(idx, self.faces[idx])
// 	}
// }

impl<'a> From<ArrayView2<'a, Tile>> for Cube<'a> {
	fn from(grid: ArrayView2<'a, Tile>) -> Self {
		use Direction::*;
		use Heading::*;

		/// A face that we're working on figuring out where it goes and how it's pointed;
		/// we'll use its position in the grid of faces (which we get from the input) and its
		/// position relative (`d_pos`) to the previous, known face that it's adjacent to,
		/// plus the knowledge of the face it's adjacent to (which face it is and which
		/// direction its North points), to determine which face *this* is and which
		/// direction *its* North points
		struct PartialFaceDatum {
			idx: usize,
			pos: [Coord; 2],
			d_pos: [Coord; 2],
		}

		let side_len = {
			let face_area = grid
				.iter()
				.filter(|&tile| !matches!(tile, Tile::Aether))
				.count() / 6;
			// A very silly way to find `sqrt(face_area)`
			(1..=face_area).find(|n| n * n == face_area).unwrap()
		};

		let face_grid_n_rows = grid.nrows() / side_len;
		let face_grid_n_cols = grid.ncols() / side_len;
		// Where in the original grid (the input) the faces of the cube lie
		let (face_grid_loc_to_idx, face_locs_in_grid) = {
			let mut face_grid_locs = [[0; 2]; Direction::COUNT];
			let mut face_grid_loc_to_idx = HashMap::with_capacity(Direction::COUNT);
			let mut idx = 0;

			for (r, c) in iproduct!(0..face_grid_n_rows, 0..face_grid_n_cols) {
				if !matches!(grid[[r * side_len, c * side_len]], Tile::Aether) {
					let loc = [r.cast(), c.cast()];
					face_grid_locs[idx] = loc;
					assert!(face_grid_loc_to_idx.insert(loc, idx).is_none());
					idx += 1;
				}
			}
			(face_grid_loc_to_idx, face_grid_locs)
		};

		// Array of CubeFace; starts with impossible dummy data
		let mut faces = [CubeFace {
			side: Yp,
			north: Yp,
		}; Direction::COUNT];

		let top_face = {
			let (first_face_pos, first_face_id) = face_grid_loc_to_idx
				.iter()
				.find_map(|(&k, &idx)| (idx == 0).then_some((k, idx)))
				.unwrap();

			PartialFaceDatum {
				idx: first_face_id,
				pos: first_face_pos,
				d_pos: Default::default(),
			}
		};

		let mut stack = vec![top_face];

		let mut seen = [false; Direction::COUNT];
		while let Some(PartialFaceDatum {
			idx,
			pos: [r, c],
			d_pos: [dr, dc],
		}) = stack.pop()
		{
			let new_face = if idx == 0 {
				CubeFace {
					side: Yp,
					north: Zn,
				}
			} else {
				let prev_face_pos = [r - dr, c - dc];
				let prev_face_idx =
					*face_grid_loc_to_idx.get(&prev_face_pos).unwrap_or_else(|| {
						panic!(
							"logic error: face at {prev_face_pos:?} was \
					 None ((r, dr, c, dc) = {:?})",
							(r, dr, c, dc)
						)
					});
				let CubeFace {
					side: prev_side,
					north: prev_north,
				} = faces[prev_face_idx];

				let prev_face_adj_edge = match [dr, dc] {
					[-1, 0] => N,
					[1, 0] => S,
					[0, -1] => W,
					[0, 1] => E,
					_ => panic!("logic error: {:?} is an invalid (dr, dc)", (dr, dc)),
				};

				CubeFace::from_adjacent_face(prev_side, prev_north, prev_face_adj_edge)
			};

			faces[idx] = new_face;

			for (adj_r, adj_c) in get_nsew_adjacent((r, c), .., ..).into_iter().flatten() {
				if let Some(&new_idx) = face_grid_loc_to_idx.get(&[adj_r, adj_c]) {
					if seen[new_idx] {
						continue;
					}
					seen[new_idx] = true;
					stack.push(PartialFaceDatum {
						idx: new_idx,
						pos: [adj_r, adj_c],
						d_pos: [adj_r - r, adj_c - c],
					});
				}
			}
		}

		Cube {
			side_len,
			grid,
			face_locs_in_grid,
			faces,
		}
	}
}

impl Cube<'_> {
	fn index_of_face_at_side(&self, side: Direction) -> usize {
		self.faces
			.iter()
			.enumerate()
			.find_map(|(i, cf)| (cf.side == side).then_some(i))
			.unwrap_or_else(|| panic!("cube {self:?} had no face at side {side:?}"))
	}

	/// Given a point in the coordinates of one face, transform the point onto an
	/// adjacent face by wrapping it around the two faces' common edge. The adjacent face
	/// is determined by the point itself — if it's beyond the north edge of the current
	/// face, then it wraps to the face adjacent to the given face's north edge, etc.
	/// ## Panics
	/// If the point does not lie on the "plus sign" formed by extending the given face
	/// a length `side_len` horizontally and vertically, as this plus sign is the surface
	/// that wraps onto adjacent faces cleanly
	fn wrap_to_next_face(&self, face_idx: usize, point: [Coord; 2]) -> [Coord; 2] {
		use Direction::*;
		use Heading::*;

		enum Thirds {
			First,
			Second,
			Third,
		}

		fn fail_plus_sign() -> ! {
			panic!(r#"`wrap_to_next_face` was given a point not on the sacred "plus sign""#)
		}

		let &Self {
			side_len, faces, ..
		} = self;
		let side_len = side_len.cast::<Coord>();

		let face = faces[face_idx];

		// The valid plus sign is five faces of a slightly larger 3x3 grid; here we get
		// the ranges of each third of this grid
		let ranges = [
			(-side_len..0, Thirds::First),
			(0..side_len, Thirds::Second),
			(side_len..2 * side_len, Thirds::Third),
		];

		let [r_third, c_third] = point.map(|coord| {
			ranges
				.iter()
				.find_map(|(range, third)| range.contains(&coord).then_some(third))
		});

		let (Some(r_third), Some(c_third)) = (r_third, c_third) else {
			fail_plus_sign();
		};
		let heading_to_adj_face = match (r_third, c_third) {
			(Thirds::Second, Thirds::Second) => return point, // no wrapping at all
			(Thirds::First, Thirds::Second) => N,
			(Thirds::Second, Thirds::First) => W,
			(Thirds::Second, Thirds::Third) => E,
			(Thirds::Third, Thirds::Second) => S,
			_ => fail_plus_sign(),
		};

		let adj_face_side = face.get_side_of_adjacent_face(heading_to_adj_face);
		let adj_face_index = self.index_of_face_at_side(adj_face_side);
		let adj_face_north = faces[adj_face_index].north;

		// If this face were centralized, then...
		let transform = face.get_centralizing_transform();

		let wrapped_point = {
			let adj_face_north = transform.apply(adj_face_north);
			let s = side_len; // short for side_len
			let m = s - 1; // the maximum index of a side, which is side_len - 1
			let [r, c] = point;

			match (heading_to_adj_face, adj_face_north) {
				(N, Xn) => [c, -r - 1],
				(N, Xp) => [-c + m, r - s],
				(N, Zn) => [r + s, c],
				(N, Zp) => [-r - 1, -c + m],
				(S, Xn) => todo!(),
				(S, Xp) => todo!(),
				(S, Zn) => todo!(),
				(S, Zp) => todo!(),
				(E, Yn) => todo!(),
				(E, Yp) => todo!(),
				(E, Zn) => todo!(),
				(E, Zp) => todo!(),
				(W, Yn) => todo!(),
				(W, Yp) => todo!(),
				(W, Zn) => todo!(),
				(W, Zp) => todo!(),

				(N | S, Yn | Yp) | (E | W, Xn | Xp) => {
					panic!(
						"the face at {h:?} was pointing {a:?}, which is impossible",
						h = heading_to_adj_face,
						a = adj_face_north
					)
				}
			}
		};

		// But it's not, so:

		// match (heading_to_adj_face, adj_face_north) {
		// 	(N, Xn) => todo!(),
		// 	(N, Xp) => todo!(),
		// 	(N, Yn) => todo!(),
		// 	(N, Yp) => todo!(),
		// 	(N, Zn) => todo!(),
		// 	(N, Zp) => todo!(),
		// 	(S, Xn) => todo!(),
		// 	(S, Xp) => todo!(),
		// 	(S, Yn) => todo!(),
		// 	(S, Yp) => todo!(),
		// 	(S, Zn) => todo!(),
		// 	(S, Zp) => todo!(),
		// 	(E, Xn) => todo!(),
		// 	(E, Xp) => todo!(),
		// 	(E, Yn) => todo!(),
		// 	(E, Yp) => todo!(),
		// 	(E, Zn) => todo!(),
		// 	(E, Zp) => todo!(),
		// 	(W, Xn) => todo!(),
		// 	(W, Xp) => todo!(),
		// 	(W, Yn) => todo!(),
		// 	(W, Yp) => todo!(),
		// 	(W, Zn) => todo!(),
		// 	(W, Zp) => todo!(),
		// }

		todo!()
	}
}

fn pt2(map: &Map) -> Ans {
	// impl Cube {
	// 	fn face_at(self, row: i32, col: i32) -> Direction {
	// 		use Direction::*;
	// 		let side_len = self.0;
	// 		match (row / side_len, col / side_len) {
	// 			(0, 1) => Yp,
	// 			(0, 2) => Xp,
	// 			(1, 1) => Zn,
	// 			(2, 0) => Xn,
	// 			(2, 1) => Yn,
	// 			(3, 0) => Zp,
	// 			_ => panic!(
	// 				"invalid face: {rc:?} (side_len={side_len})",
	// 				rc = (row, col)
	// 			),
	// 		}
	// 	}

	// 	fn translate_btwn_faces(
	// 		self,
	// 		face_row: i32,
	// 		face_col: i32,
	// 		from: Direction,
	// 		to: Direction,
	// 	) -> (i32, i32) {
	// 		let edges = from.adj_edges(to);
	// 		let side_len = self.0;
	// 		let max_idx = side_len - 1;
	// 		match edges {
	// 			(N, S) | (S, N) | (E, W) | (W, E) => (face_row, face_col),

	// 			(N, N) => (1 - face_row, max_idx - face_col),
	// 			(W, W) => (max_idx - face_row, 1 - face_col),
	// 			(S, S) => (side_len + max_idx - face_row, max_idx - face_col),
	// 			(E, E) => (max_idx - face_row, side_len + max_idx - face_col),

	// 			(N, E) | (S, W) => (max_idx - face_col, face_row - side_len),
	// 			(E, N) | (W, S) => (face_col - side_len, max_idx - face_row),

	// 			(N, W) => (face_col, max_idx - side_len - face_row),
	// 			(W, N) => (face_col - side_len, face_row),
	// 			(S, E) => (face_col, side_len + max_idx - face_row),
	// 			(E, S) => (side_len + max_idx - face_col, face_row),
	// 		}
	// 	}

	// 	fn rel_to_abs(self, face: Direction, face_row: i32, face_col: i32) -> (i32, i32) {
	// 		let (row_idx, col_idx) = face.row_col_index();
	// 		let abs_row = row_idx * self.0 + face_row;
	// 		let abs_col = col_idx * self.0 + face_col;
	// 		(abs_row, abs_col)
	// 	}

	// 	fn abs_to_rel(self, face: Direction, abs_row: i32, abs_col: i32) -> (i32, i32) {
	// 		let (face_row_idx, face_col_idx) = face.row_col_index();
	// 		let face_first_row = face_row_idx * self.0;
	// 		let face_first_col = face_col_idx * self.0;
	// 		(abs_row - face_first_row, abs_col - face_first_col)
	// 	}
	// }

	let Map { grid, steps, .. } = map;

	let cube = Cube::from(grid.view());
	println!("{cube:?}");

	// println!("{face_grid:?}");
	// println!("{cube:?}");

	// let cube = Cube(0);

	// let mut row = 0_i32;
	// let mut col = grid
	// 	.row(0)
	// 	.iter()
	// 	.position(|tile| matches!(tile, Tile::Open))
	// 	.expect("no open tile found in first row")
	// 	.cast::<i32>();

	// let n_rows = grid.nrows().cast::<i32>();
	// let n_cols = grid.ncols().cast::<i32>();

	// let mut heading = Heading::E;

	// 'this_step: for step in steps {
	// 	println!("{:?}", (row, col));
	// 	match step {
	// 		&Step::Forward(n_steps) => {
	// 			for _ in 0..n_steps {
	// 				let dr = match heading {
	// 					N => -1,
	// 					S => 1,
	// 					E | W => 0,
	// 				};
	// 				let dc = match heading {
	// 					W => -1,
	// 					E => 1,
	// 					N | S => 0,
	// 				};

	// 				let mut new_row = row + dr;
	// 				let mut new_col = col + dc;

	// 				let new_tile =
	// 					if (0..n_rows).contains(&new_row) && (0..n_cols).contains(&new_col) {
	// 						grid[[new_row.cast(), new_col.cast()]]
	// 					} else {
	// 						Tile::Aether
	// 					};

	// 				println!("{:?}", ((row, col), (new_row, new_col), new_tile));

	// 				match new_tile {
	// 					Tile::Open => {}
	// 					Tile::Solid => continue 'this_step,
	// 					Tile::Aether => {
	// 						let curr_face = cube.face_at(row, col);
	// 						let new_face = curr_face.face_at_heading(heading);
	// 						let (curr_face_row, curr_face_col) =
	// 							cube.abs_to_rel(curr_face, new_row, new_col);
	// 						let (new_face_row, new_face_col) = cube.translate_btwn_faces(
	// 							curr_face_row,
	// 							curr_face_col,
	// 							curr_face,
	// 							new_face,
	// 						);
	// 						(new_row, new_col) =
	// 							cube.rel_to_abs(new_face, new_face_row, new_face_col);
	// 					}
	// 				}

	// 				row = new_row;
	// 				col = new_col;
	// 			}
	// 		}
	// 		Step::TurnLeft => heading = heading.turned_left(),
	// 		Step::TurnRight => heading = heading.turned_right(),
	// 	}
	// }

	0
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
			&read_input(&read_file!("sample_input.txt")).unwrap(),
			(pt1, 6032),
			(pt2, 301),
		);
		run_tests(
			&read_input(&read_file!("input.txt")).unwrap(),
			(pt1, 1484),
			(pt2, 123),
		);
	}
}
