import Std
import Aoc

inductive CardLabel
| A | K | Q | J | ten | nine | eight | seven | six | five | four | three | two | W
deriving DecidableEq, Inhabited

def CardLabel.strength : CardLabel → Nat
| A => 14 | K => 13 | Q => 12 | J => 11 | ten => 10 | nine => 9 | eight => 8 | seven => 7 | six => 6
| five => 5 | four => 4 | three => 3 | two => 2 | W => 1

instance : LT CardLabel where lt a b := a.strength < b.strength
instance : DecidableRel (λ a b : CardLabel => a < b) := λ _ _ => Nat.decLt _ _
instance : Ord CardLabel where compare x y := compareOfLessAndEq x y

def Char.toCardLabel! : Char → CardLabel
| 'A' => .A | 'K' => .K | 'Q' => .Q | 'J' => .J
| 'T' => .ten | '9' => .nine | '8' => .eight | '7' => .seven | '6' => .six
| '5' => .five | '4' => .four | '3' => .three | '2' => .two
| _ => unreachable!

inductive HandType
| fiveOfaKind | fourOfaKind | fullHouse | threeOfaKind | twoPair | onePair | highCard
deriving DecidableEq, Inhabited

def HandType.strength : HandType → Nat
| fiveOfaKind => 7 | fourOfaKind => 6 | fullHouse => 5 | threeOfaKind => 4
| twoPair => 3 | onePair => 2 | highCard => 1

instance : LT HandType where lt a b := a.strength < b.strength
instance : DecidableRel (λ a b : HandType => a < b) := λ _ _ => Nat.decLt _ _
instance : Ord HandType where compare x y := compareOfLessAndEq x y

def Array.counts [DecidableEq α] (l : Array α) :=
  l.map λ x => l.map (if x = . then 1 else 0) |>.foldr (. + .) 0

abbrev Hand := Array CardLabel
def Hand.type (h : Hand) : HandType :=
  let counts := h.counts.qsortOrd
        if counts[4]! = 5 then .fiveOfaKind
  else  if counts[4]! = 4 then .fourOfaKind
  else  if counts[0]! = 2 ∧ counts[4]! = 3 then .fullHouse
  else  if counts[0]! = 1 ∧ counts[4]! = 3 then .threeOfaKind
  else  if counts[1]! = 2 ∧ counts[4]! = 2 then .twoPair
  else  if counts[2]! = 1 ∧ counts[4]! = 2 then .onePair
  else  if counts[4]! = 1 then .highCard
  else unreachable!

instance : LT Hand where lt a b := a.type < b.type ∨ (a.type = b.type ∧ a.toList < b.toList)
instance : DecidableRel (λ a b : Hand => a < b) := λ _ _ => show Decidable $ _ ∨ (_ ∧ _) from inferInstance
instance : Ord Hand where compare x y := compareOfLessAndEq x y

def String.toHand! (s : String) : Hand := s.1.toArray.map Char.toCardLabel!

structure HandWithBid where
  hand : Hand
  bid : Nat
deriving DecidableEq, Inhabited
instance : Ord HandWithBid := ⟨λ a b => lexOrd.compare (a.hand, a.bid) (b.hand, b.bid)⟩

def part1 (lines : Array String) :=
  lines.map (λ l => l.splitOn " " |> λ t => HandWithBid.mk t[0]!.toHand! t[1]!.toNat!)
  |>.qsortOrd
  |>.mapIdx (λ i h => (i.val + 1) * h.bid)
  |>.foldr (. + .) 0

def Hand.JasWildCards (h : Hand) : Hand := h.map λ | .J => .W | c => c

-- should not replace wildcards, wrong tie breaking rule, instead we need to modify Hand.type's count calculation
def Hand.replaceWildCards (h : Hand) : Hand :=
  let _ : Ord (Nat × CardLabel) := lexOrd
  let (_, maxLabelWithMaxCount) :=
    ((h.counts.zip h).map λ | (_, .W) => (0, CardLabel.W) | z => z).max?.get!
  h.map λ | .W => maxLabelWithMaxCount | c => c

def part2 (lines : Array String) :=
  lines.map (λ l => l.splitOn " " |>
    λ t => HandWithBid.mk t[0]!.toHand!.JasWildCards.replaceWildCards t[1]!.toNat!)
  |>.qsortOrd
  |>.mapIdx (λ i h => (i.val + 1) * h.bid)
  |>.foldr (. + .) 0

def main : IO Unit := do
  let lines ← IO.FS.lines "Year2023/Day07/ex1.txt"
  -- let lines ← IO.FS.lines "Year2023/Day07/in1.txt"
  IO.println s!"part1: {part1 lines}"
  IO.println s!"part2: {part2 lines}"

-- #eval timeit "main : " $ main
