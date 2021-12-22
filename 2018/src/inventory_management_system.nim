type ProblemLine = string

import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
  line

import tables

var withDoubles = 0
var withTriples = 0

for id in problem:
    var letters = initTable[char, int]()
    for letter in id:
        letters[letter] = if contains(letters, letter): letters[letter] + 1
            else: 1
    var doubles = false
    var triples = false
    for letter in keys letters:
        if letters[letter] == 2: doubles = true
        if letters[letter] == 3: triples = true
    withDoubles = if doubles: withDoubles + 1 else: withDoubles
    withTriples = if triples: withTriples + 1 else: withTriples

echo withDoubles * withTriples

func hamming(first, second: string): int =
    result = 0
    for i in countup(0, (len first) - 1):
        result = if first[i] == second[i]: result else: result + 1

for i, firstId in pairs problem:
    for secondId in problem[i..^1]:
        if hamming(firstId, secondId) == 1:
            echo firstId
            echo secondId
