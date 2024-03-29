import Std
import Aoc

def part1 (lines : Array String) := lines.size

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day08/ex1.txt"
  -- let lines ← IO.FS.lines "Year2023/Day08/in1.txt"
  IO.println s!"part1: {part1 lines}"

-- #eval timeit "main : " $ main
