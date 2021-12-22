type ProblemLine = (char, int)

import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
  (line[0], parseInt line[1..^1])

func changeFrequency(start: int, change: (char, int)): int =
  case change[0]
  of '+':
    start + change[1]
  of '-':
    start - change[1]
  else:
    raise newException(AssertionError, "invalid leading character")

echo foldl(problem, changeFrequency(a, b), 0)

import sets

func firstRevisited(
  changes: seq[(char, int)]
): int =
    var changeIndex = 0
    var frequency = 0
    var visited = initSet[int]()
    while not visited.contains frequency:
      visited.incl frequency
      frequency = frequency.changeFrequency changes[changeIndex]
      changeIndex = if changeIndex >= len(changes) - 1: 0
                    else: changeIndex + 1
    frequency

echo firstRevisited problem
