# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%202:%20Inventory%20Management%20System][Day 2: Inventory Management System:5]]
# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-2-problem-line][day-2-problem-line]]
type ProblemLine = string
# day-2-problem-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::read-problem-lines][read-problem-lines]]
import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
# read-problem-lines ends here
  # [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-2-parse-line][day-2-parse-line]]
  line
  # day-2-parse-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-2-solution-1][day-2-solution-1]]
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
# day-2-solution-1 ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-2-solution-2][day-2-solution-2]]
func hamming(first, second: string): int =
    result = 0
    for i in countup(0, (len first) - 1):
        result = if first[i] == second[i]: result else: result + 1

for i, firstId in pairs problem:
    for secondId in problem[i..^1]:
        if hamming(firstId, secondId) == 1:
            echo firstId
            echo secondId
# day-2-solution-2 ends here
# Day 2: Inventory Management System:5 ends here
