import Std.Data.Array.Init.Basic

namespace String

def reverse (s : String) := mk s.data.reverse

def toArray (s : String) : Array Char := s.data.toArray

end String


namespace Array

/-- The array `#[a, a + 1, ..., b - 1]`. -/
def natRange (a b : Nat) := (range (b - a)).map (. + a)

def sum [Add α] [OfNat α 0] (a : Array α) := a.foldl (. + .) 0

end Array
