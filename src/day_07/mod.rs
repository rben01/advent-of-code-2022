// tag::setup[]
use crate::{utils::into_rc_rc, Answer};
use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

fn ans_for_input(input: &str) -> Answer<usize, usize> {
	let dirs = read_input(input).expect("couldn't read input");
	(7, (pt1(&dirs), pt2(&dirs))).into()
}

pub fn ans() -> Answer<usize, usize> {
	ans_for_input(include_str!("input.txt"))
}

type DirectoryContentsInProgress = BTreeMap<String, DirEntryInProgress>;

#[derive(Debug)]
enum DirEntryInProgressKind {
	File,
	Directory(Rc<RefCell<DirectoryContentsInProgress>>),
}

impl DirEntryInProgressKind {
	fn new_dir_contents() -> DirectoryContentsInProgress {
		BTreeMap::new()
	}

	fn new_dir() -> Self {
		Self::Directory(into_rc_rc(Self::new_dir_contents()))
	}
}

#[derive(Debug)]
struct DirEntryInProgress {
	size: usize,
	kind: DirEntryInProgressKind,
}

impl DirEntryInProgress {
	fn new_dir() -> Self {
		Self {
			size: 0,
			kind: DirEntryInProgressKind::new_dir(),
		}
	}

	fn compute_child_sizes(&mut self) {
		if let DirEntryInProgressKind::Directory(d) = &self.kind {
			for child in d.borrow_mut().values_mut() {
				child.compute_child_sizes();
			}
			self.size = d.borrow().values().map(|entry| entry.size).sum();
		}
	}

	fn freeze(self) -> DirEntry {
		let DirEntryInProgress {
			size,
			kind: kind_in_progress,
		} = self;

		let kind = match kind_in_progress {
			DirEntryInProgressKind::File => DirEntryKind::File,
			DirEntryInProgressKind::Directory(d) => DirEntryKind::Directory(
				d.take()
					.into_iter()
					.map(|(k, child)| (k, child.freeze()))
					.collect(),
			),
		};

		DirEntry { size, kind }
	}
}

type DirectoryContents = BTreeMap<String, DirEntry>;

#[derive(Debug)]
enum DirEntryKind {
	File,
	Directory(DirectoryContents),
}

#[derive(Debug)]
struct DirEntry {
	size: usize,
	kind: DirEntryKind,
}

impl From<DirEntryInProgress> for DirEntry {
	fn from(mut root: DirEntryInProgress) -> Self {
		root.compute_child_sizes();
		root.freeze()
	}
}

fn read_input(input: &str) -> Option<DirEntry> {
	let root = into_rc_rc(DirectoryContentsInProgress::new());
	let mut dir_stack = Vec::new();

	for line in input.lines() {
		let curr_dir = dir_stack.last().map_or_else(|| Rc::clone(&root), Rc::clone);

		let mut comps = line.split_whitespace();
		let first = comps.next()?;
		match first {
			"$" => match comps.next()?.to_lowercase().as_str() {
				"cd" => {
					let arg = comps.next()?;
					match arg {
						"/" => {
							dir_stack.clear();
						}
						".." => {
							dir_stack.pop()?;
						}
						dst_name => {
							let dst = match curr_dir.borrow().get(dst_name) {
								Some(entry) => match &entry.kind {
									DirEntryInProgressKind::File => {
										panic!("tried to `cd {dst_name}`, but {dst_name} is a file")
									}
									DirEntryInProgressKind::Directory(d) => Rc::clone(d),
								},
								None => into_rc_rc(DirEntryInProgressKind::new_dir_contents()),
							};

							dir_stack.push(dst);
						}
					}
				}
				"ls" => {
					// Don't actually have to do anything here; we can just parse the output
					// that comes after (all output comes from `ls`)
				}
				cmd => panic!("invalid command {cmd:?}"),
			},
			output => {
				let mut curr_dir = curr_dir.borrow_mut();

				// the result of ls
				match output {
					"dir" => {
						let dir_name = comps.next()?;
						curr_dir
							.entry(dir_name.to_owned())
							.or_insert_with(DirEntryInProgress::new_dir);
					}
					file_size_str => {
						let size = file_size_str.parse().ok()?;
						let file_name = comps.next()?;
						curr_dir
							.entry(file_name.to_owned())
							.or_insert(DirEntryInProgress {
								size,
								kind: DirEntryInProgressKind::File,
							});
					}
				}
			}
		}
	}

	Some(DirEntry::from(DirEntryInProgress {
		size: 0,
		kind: DirEntryInProgressKind::Directory(root),
	}))
}
// end::setup[]

// tag::pt1[]
fn pt1(dirs: &DirEntry) -> usize {
	let mut ans = 0;
	let mut stack = vec![dirs];

	while let Some(entry) = stack.pop() {
		let DirEntry { size, kind } = &entry;
		let size = *size;

		if let DirEntryKind::Directory(d) = kind {
			if size <= 100_000 {
				ans += size;
			}

			for child in d.values() {
				stack.push(child);
			}
		}
	}

	ans
}
// end::pt1[]

// tag::pt2[]
fn pt2(dirs: &DirEntry) -> usize {
	const MAX_SPACE: usize = 70_000_000;
	const MIN_UNUSED_SPACE: usize = 30_000_000;

	let unused_space = MAX_SPACE - dirs.size;

	let mut min_deleted_dir_size = usize::MAX;
	let mut stack = vec![dirs];

	while let Some(entry) = stack.pop() {
		let DirEntry { size, kind } = &entry;
		let size = *size;

		if let DirEntryKind::Directory(d) = kind {
			if unused_space + size >= MIN_UNUSED_SPACE && size < min_deleted_dir_size {
				min_deleted_dir_size = size;
			}

			for child in d.values() {
				stack.push(child);
			}
		}
	}

	min_deleted_dir_size
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
			(pt1, 95437),
			(pt2, 24_933_642),
		);
		run_tests(
			&read_input(include_str!("input.txt")).unwrap(),
			(pt1, 919_137),
			(pt2, 2_877_389),
		);
	}
}
