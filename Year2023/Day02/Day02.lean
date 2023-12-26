import Aoc

inductive Color | red | green | blue deriving Repr, Inhabited, DecidableEq
instance : ToString Color := ⟨reprStr⟩

def parseColor (color : String) : Color :=
  if color = "red" then .red
  else if color = "green" then .green
  else if color = "blue" then .blue
  else unreachable!

def parseLine (line : String) :=
  let id := line.splitOn ":" |>.get! 0 |>.splitOn " " |>.get! 1 |>.toNat!
  let sets := line.splitOn ":" |>.get! 1
  ( id,
    sets.splitOn ";" |>.map λ set =>
      set.splitOn "," |>.map λ countColor =>
        let tokens := countColor.trim.splitOn " "
        let count := tokens.get! 0 |>.toNat!
        let color := tokens.get! 1 |> parseColor
        (count, color))

def maxCountOf (s : List $ Nat × Color) color :=
  s.filter (λ (_, c) => c = color) |>.map Prod.fst |>.maximum?.getD 0

def validCounts s :=
  decide $ maxCountOf s .red ≤ 12 ∧ maxCountOf s .green ≤ 13 ∧ maxCountOf s .blue ≤ 14

def part1 (lines : Array String) :=
  lines.map parseLine |>.filter (λ (_, line) => validCounts line.join) |>.map Prod.fst |>.sum

def cubeOf s := maxCountOf s .red * maxCountOf s .green * maxCountOf s .blue

def part2 (lines : Array String) :=
  lines.map parseLine |>.map (λ (_, line) => line.join |> cubeOf) |>.sum

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day02/in1.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval main
