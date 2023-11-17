# --------
# AOC 22 Day 1
# in Uiua

SplitOn ← ⊕□⍜▽¯∶\+.=,
IntoGroup ← ⊜□ ≠∞. ∵(⍣parse⋅⋅∞ ⊔)

TestFile ← "data/day01-test.txt"
InputFile ← "data/day01-input.txt"

Sums ← (
  &fras
  SplitOn @\n
  IntoGroup
  ∵(/+ ⊔)
)

PartOne ← /↥ Sums
PartTwo ← /+ ↙3 ⊏⍖. Sums

PartOne InputFile
PartTwo InputFile
