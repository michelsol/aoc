import Std
import Aoc

def parseLine (line : String) :=
  let all := line.splitOn ":" |>.get! 1
  let winning := all.splitOn "|" |>.get! 0 |>.splitOn " " |> List.toArray |>.filterMap String.toNat?
  let numbers := all.splitOn "|" |>.get! 1 |>.splitOn " " |> List.toArray |>.filterMap String.toNat?
  (winning, numbers)

def cardScore (winning numbers : Array Nat) : Nat := Id.run do
  let mut wins := mkArray 100 false
  let mut r := 0
  for x in winning do
    wins := wins.set! x true
  for x in numbers do
    if wins.get! x then
      r := if r = 0 then 1 else r * 2
  return r

def part1 (lines : Array String) :=
  (lines.map parseLine).map (λ (w, n) => cardScore w n) |>.sum

def wonCardCounts (lines : Array $ Array Nat × Array Nat) : Array Nat := Id.run do
  let n := lines.size
  let mut cardCount := mkArray n 1
  for id in [0:n] do
    let cardCountOfId := cardCount.get! id
    let (winning, numbers) := lines.get! id
    let mut winCount := 0
    let mut wins := mkArray 100 false
    for x in winning do
      wins := wins.set! x true
    for x in numbers do
      if wins.get! x then
        winCount := winCount + 1
        -- cardCount.uset
        -- panic ""
        cardCount := cardCount.set! (id + winCount) $ cardCount.get! (id + winCount) + cardCountOfId
  return cardCount

def part2 (lines : Array String) :=
  lines.map parseLine |> wonCardCounts |>.sum

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day04/in1.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval main
