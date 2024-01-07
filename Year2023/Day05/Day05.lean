import Std
import Aoc

structure MapEntry where
  dst : Nat
  src : Nat
  len : Nat
deriving Repr, Inhabited
instance : ToString MapEntry := ⟨reprStr⟩

def String.toMapEntry (s : String) : MapEntry :=
  let x := s.splitOn " " |>.map String.toNat!
  { dst := x[0]!
    src := x[1]!
    len := x[2]! }

def parseMaps (lines : Array String) :=
    lines.filter (λ l => !l.isEmpty) |>.splitOnP (λ l => l.contains ':') |>.drop 2
    |>.map λ m => m.map String.toMapEntry

def mapElement (x : Nat) (map : Array MapEntry) : Nat := Id.run do
  let mut r := x
  for e in map do
    if r ≥ e.src ∧ r < e.src + e.len then
      r := (r - e.src) + e.dst
      break
  return r

def part1 (lines : Array String) :=
  let seeds := lines[0]!.splitOn " " |>.drop 1 |>.toArray |>.map String.toNat!
  let maps := parseMaps lines
  let locations := seeds.map λ seed => Id.run do
    let mut r := seed
    for map in maps do
      r := mapElement r map
    return r
  locations.min?.get!

-- implicit assumption: map is sorted and contains disjoint intervals
def mapInterval (a b : Nat) (map : Array MapEntry) : Array (Nat × Nat) := Id.run do
  let mut r := #[]
  let mut x := a
  for e in map do
    let aj := e.src
    let bj := aj + e.len
    let aj' := e.dst
    let bj' := aj' + e.len
    -- unfinished
    if x < aj then
      r := r.push (x - aj + aj', bj - aj + aj')
      x := aj
    if x ≥ aj ∧ x < bj then
      r := r.push (x - aj + aj', bj - aj + aj')
      x := bj
  return r


def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day05/ex1.txt"
  IO.println s!"part1: {part1 lines}"

-- #eval timeit "main : " $ main
