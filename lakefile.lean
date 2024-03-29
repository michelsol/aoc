import Lake
open Lake DSL

package «aoc» where
  -- add package configuration options here

lean_lib «Aoc» where
  -- add library configuration options here

@[default_target] lean_exe «2023day01» where root := `Year2023.Day01.Day01
@[default_target] lean_exe «2023day02» where root := `Year2023.Day02.Day02
@[default_target] lean_exe «2023day03» where root := `Year2023.Day03.Day03
@[default_target] lean_exe «2023day04» where root := `Year2023.Day04.Day04
@[default_target] lean_exe «2023day05» where root := `Year2023.Day05.Day05
@[default_target] lean_exe «2023day06» where root := `Year2023.Day06.Day06
@[default_target] lean_exe «2023day07» where root := `Year2023.Day07.Day07
@[default_target] lean_exe «2023day08» where root := `Year2023.Day08.Day08

-- require std from "../std4"
require std from git "https://github.com/leanprover/std4" @ "a17d191b45af7755a1824781e33798b2be128c8e"
