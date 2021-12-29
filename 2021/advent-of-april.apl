nl←'
'
lines←(nl∘≠⊆⊢)
nums←⍎¨⍤lines
words←(' '∘≠⊆⊢)¨⍤lines

base1←(↓+.>⊢↓⍨-⍤⊣)
s1a←1⍤base1⍤nums
s1b←3⍤base1⍤nums

base2←∘.{(⍎⊃⌽⍵)×⍺=⊃⊃⍵}⍨∘'fdu'
s2a←(⊢×⍥{+/1↑⍵}1∘⊖-2∘⊖)⍤base2⍤words
s2b←{f d u←↓base2⍤words⍵ ◊ +/f×+/f×+\d-u}
