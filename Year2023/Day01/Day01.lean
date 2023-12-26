import Std
import Aoc

def calibration1 (s : String) :=
  let a := s.toArray.filter Char.isDigit |>.map λ c => c.toNat - '0'.toNat
  do return (← a.get? 0) * 10 + (← a.back?)

def numbers1 := #["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
def numbers1r := numbers1.map String.reverse
def numbers2 := numbers1.mapIdx λ k x => (x, k.val + 1)
def numbers2r := numbers1r.mapIdx λ k x => (x, k.val + 1)
def numbers3 := Array.natRange 1 10 |>.map λ k => (s!"{k}", k)

def calibration2 (s : String) :=
  let sr := s.reverse
  let srl := sr.length
  let a := numbers2 ++ numbers3 |>.filterMap λ (c, d) => s.findSubstr? c |>.map λ x => (x.startPos.1, d)
  let b := numbers2r ++ numbers3 |>.filterMap λ (c, d) => sr.findSubstr? c |>.map λ x => (srl - x.startPos.1, d)
  let _ : Ord (Nat × Nat) := lexOrd
  do return (← a.min?).2 * 10 + (← b.max?).2

def part (lines : Array String) (calibration : String → Option Nat) :=
  lines.mapM calibration |>.map Array.sum

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day01/in1.txt"
  IO.println s!"part1: {part lines calibration1}"
  IO.println s!"part2: {part lines calibration2}"

-- #eval main
