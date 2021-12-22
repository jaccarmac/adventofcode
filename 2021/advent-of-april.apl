nl←'
'
nums←{⍎¨nl(≠⊆⊢)⍵}
tokens←{' '(≠⊆⊢)¨nl(≠⊆⊢)⍵}

base1←(↓+.>⊢↓⍨-⍤⊣)
s1a←1∘base1∘nums
s1b←3∘base1∘nums

base2←{'f' 'd' 'u'∘.{(⍎⊃⌽⍵)×⍺=⊃⊃⍵}⍵}
s2a←{f d u←↓base2∘tokens⍵ ◊ +/f×+/d-u}
s2b←{f d u←↓base2∘tokens⍵ ◊ +/f×+/f×+\d-u}
