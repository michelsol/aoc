import Std
import Aoc

structure MapEntry where
  dst : Interval
  src : Interval
deriving Inhabited
instance : ToString MapEntry where
  toString i := s!"\{{i.src} => {i.dst}}"

def String.toMapEntry (s : String) : MapEntry :=
  let x := s.splitOn " " |>.map String.toNat!
  { dst := ⟨x[0]!, x[0]! + x[2]!⟩
    src := ⟨x[1]!, x[1]! + x[2]!⟩}

def parseMaps (lines : Array String) :=
    lines.filter (λ l => !l.isEmpty) |>.splitOnP (λ l => l.contains ':') |>.drop 2
    |>.map λ m =>
      (m.map String.toMapEntry).qsort λ a b => a.src.1 < b.src.1

def parseSeeds (line : String) :=
  line.splitOn " " |>.drop 1 |>.toArray |>.map String.toNat!

def mapElement (x : Nat) (map : Array MapEntry) : Nat := Id.run do
  let mut r := x
  for e in map do
    if r ≥ e.src.1 ∧ r < e.src.2 then
      r := r - e.src.1 + e.dst.1
      break
  return r

def part1 (lines : Array String) :=
  let seeds := parseSeeds lines[0]!
  let maps := parseMaps lines
  let locations := seeds.map λ seed => Id.run do
    let mut r := seed
    for map in maps do
      r := mapElement r map
    return r
  locations.min?.get!

def parseSeedRanges (line : String) : Array Interval :=
  line.splitOn " " |>.drop 1 |>.toArray |>.map String.toNat!
    |>.makeProds |>.map (λ ⟨a, b⟩ => ⟨a, a + b⟩) |>.qsort λ a b => a.1 < b.1

def mapArray (a : Array Interval) (b : Array MapEntry) : Array Interval := Id.run do
  let mut r := #[]
  let mut i := 0
  let mut j := 0
  let mut x := a[0]!.1
  repeat
    let e := b[j]!
    let s := a[i]!
    let new : Interval := ⟨max s.1 e.src.1, min s.2 e.src.2⟩
    if new.1 < new.2 then
      if x < new.1 then r := r.push ⟨x, new.1⟩
      r := r.push ⟨new.1 - e.src.1 + e.dst.1, new.2 - e.src.1 + e.dst.1⟩
      x := new.2
    if j + 1 < b.size ∧ e.src.2 <= s.2 then
      j := j + 1
    else
      i := i + 1
      if x < s.2 then r := r.push ⟨x, s.2⟩
      if i >= a.size then break
      x := a[i]!.1
  return r |>.qsort λ a b => a.1 < b.1

def part2 (lines : Array String) :=
  let seeds := parseSeedRanges lines[0]!
  let maps := parseMaps lines
  let locations := Id.run do
    let mut r := seeds
    for map in maps do
      r := mapArray r map
    return r
  locations
  -- locations.get! 0 |>.1

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day05/in1.txt"
  -- IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval timeit "main : " $ main
