import Std.Data.Array.Basic
import Std.Data.List.Basic

namespace String

def reverse (s : String) := mk s.data.reverse

def toArray (s : String) : Array Char := s.data.toArray

def arrSplitOn (s p : String) : Array String := s.splitOn p |>.toArray

end String


namespace Array

/-- The array `#[a, a + 1, ..., b - 1]`. -/
def natRange (a b : Nat) := range (b - a) |>.map (. + a)

/-- The array `#[a, a + 1, ..., b - 1]`. -/
def intRange (a b : Int) := range (b - a).toNat |>.map Int.ofNat |>.map (. + a)

def sum [Add α] [OfNat α 0] (a : Array α) := a.foldl (. + .) 0

def splitOnP (P : α → Bool) (a : Array α) :=
  a.toList.splitOnP P |>.map List.toArray |>.toArray

def drop (k : Nat) (a : Array α) := a.toList.drop k |>.toArray

open Prod in
def makeProds (l : Array α) :=
  let a := l.zip (range l.size) |>.filter (snd . % 2 = 0) |>.map fst
  let b := l.zip (range l.size) |>.filter (snd . % 2 = 1) |>.map fst
  a.zip b

-- #eval #['A', 'C', 'Z', 'E', 'F', 'Y'].makeProds

end Array


structure Interval where
  first : Nat
  firstOut : Nat
deriving Inhabited
instance : ToString Interval where
  toString i := s!"[{i.1}, {i.2})"

namespace Interval

abbrev isEmpty (i : Interval) := i.2 <= i.1

def intersect (i j : Interval) : Interval := ⟨max i.1 j.1, min i.2 j.2⟩

def sortByFirst (a : Array Interval) := a.qsort λ i j => i.1 < j.1

-- assumption : a has sorted and disjoint intervals. Same for b
-- output will have sorted and disjoint intervals
def intersectArrays (a b : Array Interval) : Array Interval := Id.run do
  let mut r := #[]
  let mut i := 0
  let mut j := 0
  while i < a.size ∧ j < b.size do
    let lo := max a[i]!.1 b[j]!.1
    let hi := min a[i]!.2 b[j]!.2
    if lo < hi then r := r.push ⟨lo, hi⟩
    if a[i]!.2 < b[j]!.2 then i := i + 1
    else j := j + 1
  return r

def ensureDisjoint (a : Array Interval) : Array Interval := Id.run do
  let mut last := a[0]!
  let mut r := #[]
  for i in a.drop 1 do
    if last.2 < i.1 then
      r := r.push last
      last := i
    else
      last := ⟨last.1, max last.2 i.2⟩
  r := r.push last
  return r

-- def list1 : Array Interval := #[⟨0, 2⟩, ⟨5, 10⟩, ⟨13, 23⟩, ⟨24, 25⟩]
-- def list2 : Array Interval := #[⟨1, 5⟩, ⟨8, 12⟩, ⟨15, 24⟩, ⟨25, 26⟩]
-- #eval intersectArrays list1 list2 |> sortByFirst
-- #eval ensureDisjoint #[⟨0, 1⟩, ⟨2, 12⟩, ⟨13, 23⟩, ⟨24, 25⟩]

end Interval
