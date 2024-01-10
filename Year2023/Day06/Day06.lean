import Std
import Aoc

def howManyWins (t m : Nat) :=
      let t := t.toFloat
      let m := m.toFloat + 0.01
      let Δ := t * t - 4 * m
      let w :=
        if Δ ≥ 0 then
          let s1 := (0.5 * (t - Δ.sqrt)).ceil
          let s2 := (0.5 * (t + Δ.sqrt)).floor
          (s2 - s1) + 1
        else 0
      w.toUInt64.toNat

def part1 (lines : Array String) :=
  let times := lines.get! 0 |>.arrSplitOn " " |>.drop 1 |>.filter (λ s => !s.isEmpty) |>.map String.toNat!
  let dists := lines.get! 1 |>.arrSplitOn " " |>.drop 1 |>.filter (λ s => !s.isEmpty) |>.map String.toNat!
  times.zip dists
    |>.map (λ (t, m) => howManyWins t m)
    |>.foldl (. * .) 1

def part2 (lines : Array String) :=
  let t := lines.get! 0 |>.arrSplitOn " " |>.drop 1 |>.filter (λ s => !s.isEmpty)
    |>.map String.toArray |>.join |>.toList |> String.mk |>.toNat!
  let m := lines.get! 1 |>.arrSplitOn " " |>.drop 1 |>.filter (λ s => !s.isEmpty)
    |>.map String.toArray |>.join |>.toList |> String.mk |>.toNat!
  howManyWins t m

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day06/in1.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval timeit "main : " $ main
