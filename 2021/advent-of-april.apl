nl←'
'
nums←{⍎¨(nl∘≠⊆⊢)⍵}

base1←(↓+.>⊢↓⍨-⍤⊣)
s1a←1∘base1∘nums
s1b←3∘base1∘nums

s2a←{
  t←(' '∘≠⊆⊢)¨(nl∘≠⊆⊢)⍵
  n←⍎¨⊢/↑t
  f d u←↓'f' 'd' 'u'∘.{⍺=⊃⊃⍵}t
  +/f×n×+/(d×n)-u×n
}
s2b←{0}
