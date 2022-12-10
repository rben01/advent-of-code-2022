macro_rules! show_answers {
	($($mod_name:ident:$ft_name:literal),* $(,)?) => {
		$(
			#[cfg(feature=$ft_name)]
			{
				println!("{}", advent_of_code_2022::$mod_name::ans());
			}
		)*
	};
}

fn main() {
	show_answers!(
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
}
