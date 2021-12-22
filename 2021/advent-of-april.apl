nl←'
'
nums←{⍎¨nl(≠⊆⊢)⍵}

base1←(↓+.>⊢↓⍨-⍤⊣)
s1a←1∘base1∘nums
s1b←3∘base1∘nums

base2←{'f' 'd' 'u'∘.{(⍎⊃⌽⍵)×⍺=⊃⊃⍵}' '(≠⊆⊢)¨nl(≠⊆⊢)⍵}
s2a←{f d u←↓base2⍵ ◊ +/f×+/d-u}
s2b←{f d u←↓base2⍵ ◊ +/f×+/f×+\d-u}
