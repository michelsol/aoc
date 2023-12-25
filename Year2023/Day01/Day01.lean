-- import Std

def calibration (s : String) : Option Nat :=
  let l := (s.data.filter Char.isDigit).map λ c => c.toNat - '0'.toNat
  do
    let first ← l.get? 0
    let last ← l.getLast?
    return first * 10 + last

def part1 (lines : Array String) := do
  let l ← lines.mapM calibration
  return l.foldr (. + .) 0

def preprocess (s : String) : String := -- this is a wrong idea
  let s := s.replace "one" "1"
  let s := s.replace "two" "2"
  let s := s.replace "three" "3"
  let s := s.replace "four" "4"
  let s := s.replace "five" "5"
  let s := s.replace "six" "6"
  let s := s.replace "seven" "7"
  let s := s.replace "eight" "8"
  let s := s.replace "nine" "9"
  s

def part2 (lines : Array String) :=
  lines.map preprocess

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day01/ex1.txt"
  IO.println s!"part1: {part1 lines}"
  let lines ← IO.FS.lines "Year2023/Day01/ex2.txt"
  IO.println s!"part2: {part2 lines}"

-- #eval main
