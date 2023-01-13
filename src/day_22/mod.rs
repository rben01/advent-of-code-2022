// tag::setup[]
use crate::{
	read_file,
	utils::{get_nsew_adjacent, ArrayWrapper},
	Answer, Cast,
};
use itertools::iproduct;
use ndarray::prelude::{Array2, ArrayView2};
use std::{
	collections::HashMap,
	ops::{Index, IndexMut},
};
use strum::{EnumCount, IntoEnumIterator};
use strum_macros::{EnumCount, EnumIter};

type Coord = i32;
type Ans = i32;

fn ans_for_input(input: &str) -> Answer<Ans, Ans> {
	let nums = read_input(input).expect("could not read input");
	(22, (pt1(&nums), pt2(&nums))).into()
}

pub fn ans() -> Answer<Ans, Ans> {
	ans_for_input(&read_file!("input.txt"))
}

/// A compass heading
#[derive(Debug, Clone, Copy, EnumCount, PartialEq, Eq)]
#[repr(usize)]
enum Heading {
	N,
	E,
	S,
	W,
}

impl Heading {
	const CW_FROM_N: [Self; Self::COUNT] = [Self::N, Self::E, Self::S, Self::W];

	fn value(self) -> Ans {
		use Heading::*;
		match self {
			E => 0,
			S => 1,
			W => 2,
			N => 3,
		}
	}

	/// `N -> W -> S -> E -> N`
	fn turned_left(self, n_times: usize) -> Self {
		use Heading::*;
		let mut ans = self;
		for _ in 0..n_times {
			ans = match ans {
				N => W,
				W => S,
				S => E,
				E => N,
			}
		}
		ans
	}

	/// `N -> E -> S -> W -> N`
	fn turned_right(self, n_times: usize) -> Self {
		use Heading::*;
		let mut ans = self;
		for _ in 0..n_times {
			ans = match ans {
				N => E,
				E => S,
				S => W,
				W => N,
			}
		}
		ans
	}

	/// The number of 90° clockwise turns it would take to get from `from` to `self`.
	/// ## Examples
	/// ```rust
	/// assert_eq!(3, N.dist_clockwise(E));
	/// ```
	fn dist_clockwise(self, from: Self) -> usize {
		let a = self as usize;
		let b = from as usize;

		a.checked_sub(b).unwrap_or(a + Heading::COUNT - b)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

			if tile != Tile::Aether {
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

fn value(row: Coord, col: Coord, heading: Heading) -> Ans {
	1000 * (row + 1) + 4 * (col + 1) + heading.value()
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
		.position(|&tile| tile == Tile::Open)
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

					let sign = if heading == N { -1 } else { 1 };

					let lo = range.lo;
					for _ in 0..n_steps_before_loop {
						let next_r = lo + (row - lo + sign).rem_euclid(range.size());
						if grid[[next_r.cast(), col.cast()]] == Tile::Solid {
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

					let sign = if heading == W { -1 } else { 1 };

					let lo = range.lo;
					for _ in 0..n_steps_before_loop {
						let next_c = lo + (col - lo + sign).rem_euclid(range.size());
						if grid[[row.cast(), next_c.cast()]] == Tile::Solid {
							continue 'this_step;
						}
						col = next_c;
					}

					// If we're here, we haven't hit a closed tile yet
					col = (lo + (col - lo + sign * n_steps_after_loop).rem_euclid(range.size()))
						.cast();
				}
			},
			Step::TurnLeft => heading = heading.turned_left(1),
			Step::TurnRight => heading = heading.turned_right(1),
		}
	}

	value(row, col, heading)
}
// end::pt1[]

// tag::pt2[]
/// A direction, positive (`p`) or negative (`n`), along an axis (`X`, `Y`, or `Z`).
/// E.g., `Xp` represents pointing along the positive x-axis, `Zn` along the negative
/// z-axis, etc.
#[derive(Debug, Clone, Copy, EnumCount, PartialEq, Eq, Hash, EnumIter)]
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

/// One of six revolutions of the cube about its center that moves Zp to another face.
/// There are actually seven total — the identity, four for the adjacent faces, and
/// *two* ways to move Zp to Zn — but we only include one of the two ways that gets Zp
/// to Zn (the one that revolves about the X axis).
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

/// A rotation of the cube about the Z axis, oriented so that Zp points into our face,
/// measured in quarter turns clockwise as we look down at the Zp face
#[derive(Debug, Clone, Copy)]
enum RotationAboutZp {
	Zero,
	OneQuarterCw,
	HalfTurn,
	ThreeQuartersCw,
}

impl RotationAboutZp {
	/// Apply this rotation to the given direction
	fn apply(self, dir: Direction) -> Direction {
		use Direction::*;
		use RotationAboutZp::*;

		match (dir, self) {
			(_, Zero) => dir,
			(Xp, HalfTurn) | (Yn, OneQuarterCw) | (Yp, ThreeQuartersCw) => Xn,
			(Xn, HalfTurn) | (Yn, ThreeQuartersCw) | (Yp, OneQuarterCw) => Xp,
			(Xn, ThreeQuartersCw) | (Xp, OneQuarterCw) | (Yp, HalfTurn) => Yn,
			(Xn, OneQuarterCw) | (Xp, ThreeQuartersCw) | (Yn, HalfTurn) => Yp,
			(Zn, _) => Zn,
			(Zp, _) => Zp,
		}
	}
}

/// A transformation of the cube, modeled as a revolution (moving the Zp face elsewhere)
/// followed by a rotation (about Zp, whereever it was moved to). There are 24 total
/// transformations: move Zp to one of the six faces, then orient it in one of the four
/// orientations.
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
}

/// An oriented face of the cube; it's on one side of the cube, and its compass's North
/// is pointing in a known direction
#[derive(Debug, Clone, Copy)]
struct CubeFace {
	side: Direction,
	north: Direction,
}

impl CubeFace {
	fn fail(side: Direction, north: Direction) -> ! {
		panic!("side {side:?} cannot have its North pointing toward {north:?}")
	}

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
				CubeFace::fail(side, north)
			}
		};

		Self {
			side: my_side,
			north: my_north,
		}
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
			Xn => OneQuarterCw,
			Xp => ThreeQuartersCw,
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
}

/// Data on an adjacent face (for any given other face). Contains the relative heading
/// of the adjacent face — if you unfolded the cube to lay flat, and the given face and
/// its adjacent face were adjacent in this unfolding, what would the heading of the
/// adjacent face be relative to the given face's North? — and which side of the cube it
/// is.
#[derive(Debug, Clone, Copy)]
struct AdjFace {
	/// The heading of this adjacent face relative to current face's North
	relative_heading: Heading,
	/// The side of the cube this adjacent face is on
	side: Direction,
}

/// Data pertaining to the four faces adjacent to a given face. Indexable by `Heading`.
#[derive(Debug, Clone, Copy)]
struct AdjacentFaces<T>([T; Heading::COUNT]);

impl<T> From<[T; Heading::COUNT]> for AdjacentFaces<T> {
	fn from(value: [T; Heading::COUNT]) -> Self {
		Self(value)
	}
}

impl<A> ArrayWrapper<{ Heading::COUNT }> for AdjacentFaces<A> {
	type T = A;
	type Me<U> = AdjacentFaces<U>;

	fn wrapped(self) -> [Self::T; Heading::COUNT] {
		self.0
	}
}

impl<T> Index<Heading> for AdjacentFaces<T> {
	type Output = T;

	fn index(&self, index: Heading) -> &Self::Output {
		&self.0[index as usize]
	}
}

impl<T> IndexMut<Heading> for AdjacentFaces<T> {
	fn index_mut(&mut self, index: Heading) -> &mut Self::Output {
		&mut self.0[index as usize]
	}
}

/// Info pertaining to a given face of the cube: info on the adjacent faces, and
/// coordinates of this face in the original (flattened) grid.
#[derive(Debug, Clone, Copy)]
struct FaceDatum {
	/// Info on the adjacent faces: which side of the cube they're on and which way
	/// they're facing, relative to this face.
	adjacent_faces: AdjacentFaces<AdjFace>,
	/// The location in the grid (`[row, col]`) of face `i`, *after* the grid has been
	/// divvied up into sub-areas of size `side_len * side_len`. So the cube's bottommost
	/// face in the grid might have a `row`-index of, say, 3 or 4, and not `3 * side_len`
	/// or `4 * side_len`.
	loc_in_grid: [Coord; 2],
}

/// Data pertaining to the six faces of the cube. Indexable by `Direction`.
#[derive(Debug, Clone, Copy)]
struct Faces<T>([T; Direction::COUNT]);

impl<T> From<[T; Direction::COUNT]> for Faces<T> {
	fn from(value: [T; Direction::COUNT]) -> Self {
		Self(value)
	}
}

impl<A> ArrayWrapper<{ Direction::COUNT }> for Faces<A> {
	type T = A;
	type Me<U> = Faces<U>;

	fn wrapped(self) -> [Self::T; Direction::COUNT] {
		self.0
	}
}

impl<T> Faces<T> {
	fn map<F, U>(self, f: F) -> Faces<U>
	where
		F: FnMut(T) -> U,
	{
		Faces(self.0.map(f))
	}

	fn zip<U>(self, rhs: Faces<U>) -> Faces<(T, U)> {
		Faces(self.0.zip(rhs.0))
	}
}

impl<T: Copy> Faces<T> {
	/// Make a new array filled with `init`
	fn filled(with: T) -> Self {
		Self([with; Direction::COUNT])
	}
}

impl<T> Index<Direction> for Faces<T> {
	type Output = T;

	fn index(&self, index: Direction) -> &Self::Output {
		&self.0[index as usize]
	}
}

impl<T> IndexMut<Direction> for Faces<T> {
	fn index_mut(&mut self, index: Direction) -> &mut Self::Output {
		&mut self.0[index as usize]
	}
}

/// The whole shebang: a representation of the cube we've been given, both laid flat and
/// folded up into an actual 3D cube. Knows its side length, the 2D grid it was
/// constructed from, and most importantly, all of its faces' data: which side they're
/// on and how they're oriented relative to the faces adjacent to them.
#[derive(Debug, Clone, Copy)]
struct Cube<'a> {
	/// The side length of the cube — the faces in the grid will be `side_len * side_len`
	/// tiles
	side_len: usize,
	/// The original grid of tiles that the faces of this cube were laid flat on by
	/// unfolding the cube
	grid: ArrayView2<'a, Tile>,
	/// `face_data[direction]` is datum for face `direction`
	face_data: Faces<FaceDatum>,
}

impl<'a> From<ArrayView2<'a, Tile>> for Cube<'a> {
	/// Builds a Cube from a 2D grid
	///
	/// Tl;dr, the method is:
	/// 1. Chop the grid into face-sized pieces.
	/// 2. Arbitrarily position the first face we find (leftmost in the first nonempty
	///    row) to be Zp pointing toward Yp.
	/// 3. For each face after the first face, record its position relative to a face
	///    we've already positioned (we bfs to move from known faces to not-yet-known
	///    faces) in order to figure out which face of the cube it is and how it's oriented.
	/// 4. Reorder the list of faces so that we can index with a `Direction`, i.e., the
	///    face on side `Direction::iter().nth(i).unwrap()` is at index `i`.
	/// 5. Go through the faces, which we now know the positions and orientations of, and
	///    get the relative headings of the faces adjacent to them.
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
			let face_area = grid.iter().filter(|&&tile| tile != Tile::Aether).count() / 6;
			// A very silly way to find `sqrt(face_area)`
			(1..=face_area).find(|n| n * n == face_area).unwrap()
		};

		let face_grid_n_rows = grid.nrows() / side_len;
		let face_grid_n_cols = grid.ncols() / side_len;
		// Where in the original grid (the input) the faces of the cube lie
		let face_grid_loc_to_idx = {
			let mut face_grid_locs = [[0; 2]; Direction::COUNT];
			let mut face_grid_loc_to_idx = HashMap::with_capacity(Direction::COUNT);
			let mut idx = 0;

			for (r, c) in iproduct!(0..face_grid_n_rows, 0..face_grid_n_cols) {
				if grid[[r * side_len, c * side_len]] != Tile::Aether {
					let loc = [r.cast(), c.cast()];
					face_grid_locs[idx] = loc;
					assert!(face_grid_loc_to_idx.insert(loc, idx).is_none());
					idx += 1;
				}
			}
			face_grid_loc_to_idx
		};

		// Array of CubeFace; starts with impossible dummy data
		let mut faces_unordered = [CubeFace {
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

		// bfs the grid of cubes, filling in new faces based on their position relative to
		// the face they were adjacent to
		let mut stack = vec![top_face];
		let mut seen = [false; Direction::COUNT];
		while let Some(PartialFaceDatum {
			idx,
			pos: [r, c],
			d_pos: [dr, dc],
		}) = stack.pop()
		{
			let new_face = if idx == 0 {
				// the famed "centralized" face
				CubeFace {
					side: Zp,
					north: Yp,
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
				} = faces_unordered[prev_face_idx];

				let prev_face_adj_edge = match [dr, dc] {
					[-1, 0] => N,
					[1, 0] => S,
					[0, -1] => W,
					[0, 1] => E,
					_ => panic!("logic error: {:?} is an invalid (dr, dc)", (dr, dc)),
				};

				CubeFace::from_adjacent_face(prev_side, prev_north, prev_face_adj_edge)
			};

			faces_unordered[idx] = new_face;

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

		// At this point, `faces` is sorted more or less arbitrarily. Now we'll put it in
		// the order specified by the variants of `Direction`

		let mut grid_locs = Faces::filled(None);
		let mut norths = Faces::filled(None);

		for (loc, idx) in face_grid_loc_to_idx {
			let CubeFace { side, north } = faces_unordered[idx];
			norths[side] = Some(north);
			grid_locs[side] = Some(loc);
		}

		let grid_locs = grid_locs.map(|o| o.unwrap());
		let norths = norths.map(|o| o.unwrap());

		// For each face of the cube, get the four adjacent faces and their relative
		// headings
		let mut adjacent_faces = Faces::filled(None);
		for side in Direction::iter() {
			/// Given four sides in clockwise order, rotate them so that `first` is first
			fn get_ordered_adj_sides(
				mut sides_cw: [Direction; Heading::COUNT],
				first: Direction,
			) -> AdjacentFaces<Direction> {
				let idx = sides_cw.iter().position(|&side| side == first).unwrap();
				sides_cw.rotate_left(idx);
				AdjacentFaces(sides_cw)
			}

			let north = norths[side];

			// To minimize the number of cases we need to check, we'll do all of our work
			// on the "centralized" face, and transform the adjacent faces with the same
			// transformation. This works because the relative headings are invariant under
			// transformations of the cube. This should reduce the amount of code we have
			// to write by approximately a factor of 6.

			let centralizing_transform = CubeFace { side, north }.get_centralizing_transform();

			// The basic structure here is we list the four adjacent sides of each face in
			// clockwise order (looking at the face from outside the cube), beginning with
			// an arbitrary face, and then rotate the list as needed so that it lines up
			// with current face's North, so that it lists the faces at [N, E, S, W].
			let adj_sides_nsew = match side {
				Xn => get_ordered_adj_sides([Yn, Zn, Yp, Zp], north),
				Xp => get_ordered_adj_sides([Yn, Zp, Yp, Zn], north),
				Yn => get_ordered_adj_sides([Xn, Zp, Xp, Zn], north),
				Yp => get_ordered_adj_sides([Xn, Zn, Xp, Zp], north),
				Zn => get_ordered_adj_sides([Xn, Yn, Xp, Yp], north),
				Zp => get_ordered_adj_sides([Xn, Yp, Xp, Yn], north),
			};

			let adj_sides_relative_headings = Heading::CW_FROM_N.map(|at_heading| {
				// Treat everything as if it were relative to the centralized face — this
				// massively reduces the amount of code we have to write
				let untransformed_adj_side = adj_sides_nsew[at_heading];
				let adj_north = centralizing_transform.apply(norths[untransformed_adj_side]);
				let adj_side = centralizing_transform.apply(untransformed_adj_side);

				// beautiful cyclic structure here, both within and between cases
				match (at_heading, (adj_side, adj_north)) {
					(N, (_, Zn))
					| (E, (Xn, Yn) | (Yn, Xp) | (Xp, Yp) | (Yp, Xn))
					| (S, (_, Zp))
					| (W, (Xn, Yp) | (Yp, Xp) | (Xp, Yn) | (Yn, Xn)) => N,

					(N, (Xn, Yp) | (Yp, Xp) | (Xp, Yn) | (Yn, Xn))
					| (E, (_, Zn))
					| (S, (Xn, Yn) | (Yn, Xp) | (Xp, Yp) | (Yp, Xn))
					| (W, (_, Zp)) => E,

					(N, (_, Zp))
					| (E, (Xn, Yp) | (Yp, Xp) | (Xp, Yn) | (Yn, Xn))
					| (S, (_, Zn))
					| (W, (Xn, Yn) | (Yn, Xp) | (Xp, Yp) | (Yp, Xn)) => S,

					(N, (Xn, Yn) | (Yn, Xp) | (Xp, Yp) | (Yp, Xn))
					| (E, (_, Zp))
					| (S, (Xn, Yp) | (Yp, Xp) | (Xp, Yn) | (Yn, Xn))
					| (W, (_, Zn)) => W,

					(_, (Xn | Xp, Xn | Xp) | (Yn | Yp, Yn | Yp)) => {
						CubeFace::fail(adj_side, adj_north)
					}
					(_, (Zn | Zp, _)) => unreachable!(),
				}
			});

			let adj_sides_relative_headings = AdjacentFaces(adj_sides_relative_headings);

			adjacent_faces[side] = Some(adj_sides_relative_headings.zip(adj_sides_nsew).map(
				|(relative_heading, side)| AdjFace {
					relative_heading,
					side,
				},
			));
		}

		let adjacent_faces = adjacent_faces.map(|o| o.unwrap());

		let face_data = grid_locs
			.zip(adjacent_faces)
			.map(|(loc_in_grid, adjacent_faces)| FaceDatum {
				adjacent_faces,
				loc_in_grid,
			});

		Cube {
			side_len,
			grid,
			face_data,
		}
	}
}

/// The state as we travel through the cube: which side we're on, our coordinates on
/// that side (relative to the side's own top-left corner), and which direction we're
/// heading.
#[derive(Debug, Clone, Copy)]
struct TravelState {
	side: Direction,
	point: [Coord; 2],
	heading: Heading,
}

impl Cube<'_> {
	fn get_grid_coords(&self, side: Direction, point: [Coord; 2]) -> [usize; 2] {
		let &Self {
			side_len,
			face_data,
			grid: _,
		} = self;
		face_data[side]
			.loc_in_grid
			.zip(point)
			.map(|(face_coord, offset)| (face_coord * side_len.cast::<Coord>() + offset).cast())
	}

	fn tile_at(&self, side: Direction, point: [Coord; 2]) -> Tile {
		let coords = self.get_grid_coords(side, point);
		self.grid.get(coords).copied().unwrap_or(Tile::Aether)
	}

	/// Try to step forward one space, wrapping around the edge of the cube as needed.
	/// Returns the new `TravelState` (wrapped in `Some`) if we succeeded in moving
	/// forward. Otherwise, we hit a solid tile and didn't move, and this returns `None`.
	fn step_forward(&self, travel_state: TravelState) -> Option<TravelState> {
		use Heading::*;

		let TravelState {
			point: [r, c],
			side,
			heading,
		} = travel_state;

		let &Self {
			side_len,
			face_data,
			grid: _,
		} = self;
		let side_len = side_len.cast::<Coord>();
		let face = face_data[side];

		let [dr, dc] = match heading {
			N => [-1, 0],
			E => [0, 1],
			S => [1, 0],
			W => [0, -1],
		};
		let new_r = r + dr;
		let new_c = c + dc;

		let tentative_state = if [new_r, new_c]
			.into_iter()
			.all(|x| (0..side_len).contains(&x))
		{
			// Remain on same face
			TravelState {
				side,
				point: [new_r, new_c],
				heading,
			}
		} else {
			// We've gone over the edge and wrapped around to another face

			// new_r and new_c as they would be on the adjacent face we're wrapping to, if
			// said face's North were the same direction as the current face's
			let [unrotated_r, unrotated_c] = match heading {
				N => [new_r + side_len, new_c],
				E => [new_r, new_c - side_len],
				S => [new_r - side_len, new_c],
				W => [new_r, new_c + side_len],
			};

			let AdjFace {
				relative_heading: adj_relative_heading,
				side: adj_side,
			} = face.adjacent_faces[heading];

			// But said face's North need not be the same direction as the current face's,
			// so adjust for the heading of the adjacent face we're moving to
			let max_idx = side_len - 1;
			let rotated_rc = match adj_relative_heading {
				N => [unrotated_r, unrotated_c],
				E => [max_idx - unrotated_c, unrotated_r],
				S => [max_idx - unrotated_r, max_idx - unrotated_c],
				W => [unrotated_c, max_idx - unrotated_r],
			};

			let n_turns_cw = heading.dist_clockwise(adj_relative_heading);
			let new_heading = N.turned_right(n_turns_cw);

			TravelState {
				side: adj_side,
				point: rotated_rc,
				heading: new_heading,
			}
		};

		match self.tile_at(tentative_state.side, tentative_state.point) {
			Tile::Aether => {
				unreachable!("logic error: should not be at an aether tile after wrapping faces")
			}
			Tile::Open => Some(tentative_state),
			Tile::Solid => None,
		}
	}
}

fn pt2(map: &Map) -> Ans {
	use Direction::*;
	use Heading::*;

	let Map { grid, steps, .. } = map;

	let cube = Cube::from(grid.view());

	// By construction, the Zp face of the cube contains the starting point
	let mut curr_state = TravelState {
		side: Zp,
		point: [0, 0],
		heading: E,
	};

	for step in steps {
		match step {
			&Step::Forward(n) => {
				for _ in 0..n {
					let Some(next_state) = cube.step_forward(curr_state) else {
						break;
					};
					curr_state = next_state;
				}
			}
			Step::TurnLeft => curr_state.heading = curr_state.heading.turned_left(1),
			Step::TurnRight => curr_state.heading = curr_state.heading.turned_right(1),
		}
	}

	let TravelState {
		side,
		point,
		heading,
	} = curr_state;

	let [row, col] = cube.get_grid_coords(side, point).map(|x| x.cast());

	value(row, col, heading)
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
			(pt2, 5031),
		);
		run_tests(
			&read_input(&read_file!("input.txt")).unwrap(),
			(pt1, 1484),
			(pt2, 142_228),
		);
	}
}
