# BQN utilities

⟨StringToInt, ReadNumbers, SplitOn⟩⇐

StringToInt ← 10⊸×⊸+˜´∘⌽-⟜'0'
ReadNumbers ← StringToInt¨ ∘ •file.Lines
SplitOn ← (⊢-˜+`×¬)∘=⊔⊢

# The End
