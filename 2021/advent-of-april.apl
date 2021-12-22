nl←'
'
nums←{⍎¨(nl∘≠⊆⊢)⍵}

base1←(↓+.>⊢↓⍨-⍤⊣)
s1a←1∘base1∘nums
s1b←3∘base1∘nums

s2a←{
  t←(' '∘≠⊆⊢)¨(nl∘≠⊆⊢)⍵
  f d u←↓'f' 'd' 'u'∘.{(⍎⊃⌽⍵)×⍺=⊃⊃⍵}t
  +/f×+/d-u
}
s2b←{0}
