import Std
import Aoc

def parse (lines : Array String) := lines.map String.toArray

def Char.isSymbol (c : Char) := !c.isAlphanum && c != '.'

instance : Coe Int Nat := ⟨Int.toNat⟩

def listNumbersAdjToCellSatisfying (a : Array $ Array Char) (p : Char → Bool)
  (localRescan : Bool) : Array $ Nat × Nat × Array Nat := Id.run do
  let (m, n) := (a.size, a[0]!.size)
  let mut scanned : Array $ Array Bool := .mkArray m $ .mkArray n false
  let mut r := .mkEmpty 0
  for i in Array.intRange 0 m do
    for j in Array.intRange 0 n do
      if not $ p $ a.get! i |>.get! j then continue
      let mut numbers := .mkEmpty 0
      if localRescan then
        for i' in #[i - 1, i, i + 1] do
          for j' in #[j - 1, j, j + 1] do
            scanned := scanned.set! i' $ scanned.get! i' |>.set! j' false
      for i' in #[i - 1, i, i + 1] do
        for j' in #[j - 1, j, j + 1] do
          if scanned.get! i' |>.get! j' then continue
          if not $ Char.isDigit $ a.get! i' |>.get! j' then continue
          let mut j'' := j'
          repeat
            if j'' = 0 then break
            if not $ Char.isDigit $ a.get! i' |>.get! (j'' - 1) then break
            j'' := j'' - 1
          let mut number := 0
          repeat
            if j'' = n then break
            let c := a.get! i' |>.get! j''
            if not c.isDigit then break
            scanned := scanned.set! i' $ scanned.get! i' |>.set! j'' true
            number := number * 10 + (c.toNat - '0'.toNat)
            j'' := j'' + 1
          numbers := numbers.push number
      r := r.push (i, j, numbers)
  return r

def part1 (lines : Array String) :=
  listNumbersAdjToCellSatisfying (parse lines) Char.isSymbol false |>.map (λ (_, _, a) => a) |>.join |>.sum

def part2 (lines : Array String) :=
  listNumbersAdjToCellSatisfying (parse lines) (. = '*') true
    |>.map (λ (_, _, a) => a)
    |>.map (λ | ⟨[a, b]⟩ => a * b | _ => 0)
    |>.sum

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day03/in1.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval main
