import Std.Data.Array.Init.Basic
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

end Array
