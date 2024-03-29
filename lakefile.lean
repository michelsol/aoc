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

require std from git "https://github.com/leanprover/std4" @ "ee49cf8fada1bf5a15592c399a925c401848227f"
require Qq from git "https://github.com/leanprover-community/quote4" @ "ccba5d35d07a448fab14c0e391c8105df6e2564c"
require aesop from git "https://github.com/leanprover-community/aesop" @ "69404390bdc1de946bf0a2e51b1a69f308e56d7a"
require proofwidgets from git "https://github.com/leanprover-community/ProofWidgets4" @ "31d41415d5782a196999d4fd8eeaef3cae386a5f"
require Cli from git "https://github.com/leanprover/lean4-cli" @ "a751d21d4b68c999accb6fc5d960538af26ad5ec"
-- require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "93e820f3619d1e6ec775246653c587c04439de0c"
