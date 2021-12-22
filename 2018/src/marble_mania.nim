# [[file:../advent-of-nim.org::*Day 9: Marble Mania][Day 9: Marble Mania:6]]
# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-9-problem-line][day-9-problem-line]]][day-9-problem-line]]
type ProblemLine = tuple[players, highMarble: int]
# day-9-problem-line ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::read-problem-lines][read-problem-lines]]][read-problem-lines]]
import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
# read-problem-lines ends here
  # [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-9-parse-line][day-9-parse-line]]][day-9-parse-line]]
  let tokens = line.split ' '
  (parseInt tokens[0], parseInt tokens[6])
  # day-9-parse-line ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-9-solution-1][day-9-solution-1]]][day-9-solution-1]]
let players = problem[0].players
let highMarble = problem[0].highMarble

type Marble = ref object
  value: int
  clockwise: Marble
  counterClockwise: Marble

func playGame(players, highMarble: int): seq[int] =
  for _ in 1.countup players:
    result.add 0
  var currentPlayer = 0
  var currentMarble = new Marble
  currentMarble.value = 0
  currentMarble.clockwise = currentMarble
  currentMarble.counterClockwise = currentMarble
  for marble in 1.countup highMarble:
    if marble mod 23 == 0:
      result[currentPlayer] += marble
      for _ in 1.countup 7:
        currentMarble = currentMarble.counterClockwise
      result[currentPlayer] += currentMarble.value
      currentMarble.counterClockwise.clockwise = currentMarble.clockwise
      currentMarble.clockwise.counterClockwise = currentMarble.counterClockwise
      currentMarble = currentMarble.clockwise
    else:
      var newMarble = new Marble
      newMarble.value = marble
      currentMarble = currentMarble.clockwise
      currentMarble.clockwise.counterClockwise = newMarble
      newMarble.clockwise = currentMarble.clockwise
      currentMarble.clockwise = newMarble
      newMarble.counterClockwise = currentMarble
      currentMarble = currentMarble.clockwise
    inc currentPlayer
    if currentPlayer == len result: currentPlayer = 0

echo max playGame(players, highMarble)
# day-9-solution-1 ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-9-solution-2][day-9-solution-2]]][day-9-solution-2]]
echo max playGame(players, highMarble * 100)
# day-9-solution-2 ends here
# Day 9: Marble Mania:6 ends here
