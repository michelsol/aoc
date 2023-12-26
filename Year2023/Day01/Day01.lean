import Std

def calibration (s : String) : Option Nat :=
  let l := (s.data.filter Char.isDigit).map λ c => c.toNat - '0'.toNat
  do
    let first ← l.get? 0
    let last ← l.getLast?
    return first * 10 + last

def part1 (lines : Array String) := do
  let l ← lines.mapM calibration
  return l.foldr (. + .) 0

def String.reverse (s : String) := String.mk s.data.reverse
def numbers := #["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
def numbers2 := ((Array.range 9).map (. + 1)).map λ k => (s!"{k}", k)
def ns := numbers.mapIdx λ k x => (x, k.val + 1)
def nsRev := (numbers.map String.reverse).mapIdx λ k x => (x, k.val + 1)

def calibration2 (s : String) : Option Nat :=
  let sr := s.reverse
  let srlength := sr.length
  let a := (ns ++ numbers2).filterMap λ (ds, d) => (s.findSubstr? ds).map λ x => (x.startPos.1, d)
  let b := (nsRev ++ numbers2).filterMap λ (ds, d) => (sr.findSubstr? ds).map λ x => (srlength - x.startPos.1, d)
  let _ : Ord (Nat × Nat) := lexOrd
  do
    let first ← a.min?
    let last ← b.max?
    return first.2 * 10 + last.2

def part2 (lines : Array String) := do
  let l ← lines.mapM calibration2
  return l.foldr (. + .) 0

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day01/in.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval main
