type ProblemLine = tuple[players, highMarble: int]

import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
  let tokens = line.split ' '
  (parseInt tokens[0], parseInt tokens[6])

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

echo max playGame(players, highMarble * 100)
