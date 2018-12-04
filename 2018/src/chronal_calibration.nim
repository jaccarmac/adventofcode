# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%201:%20Chronal%20Calibration][Day 1: Chronal Calibration:7]]
import os
import sequtils
import sets
import strutils

let changes = map(splitLines(
  if paramCount() > 0: (readFile paramStr 1)
  else: readAll stdin
)[0..^2]) do (line: string) -> (char, int): (line[0], parseInt line[1..^1])

func changeFrequency(start: int, change: (char, int)): int =
  case change[0]
  of '+':
    start + change[1]
  of '-':
    start - change[1]
  else:
    raise newException(AssertionError, "invalid leading character")

echo foldl(changes, changeFrequency(a, b), 0)

func firstRevisited(
  changes: seq[(char, int)], changeIndex, frequency: int, visited: HashSet[int]
): (int, int, HashSet[int]) =
    let newChangeIndex = if changeIndex >= len(changes) - 1: 0
                         else: changeIndex + 1
    let newFrequency = changeFrequency(frequency, changes[changeIndex])
    var newVisited = initSet[int]()
    incl newVisited, frequency
    if contains(visited, newFrequency): (changeIndex, newFrequency, visited)
    else: (newChangeIndex, newFrequency, visited + newVisited)

func firstRevisited(changes: seq[(char, int)]): int =
  var changeIndex = 0
  var frequency = 0
  var visited = initSet[int]()
  while true:
    let nextArgs = firstRevisited(changes, changeIndex, frequency, visited)
    if nextArgs[0] == changeIndex: return nextArgs[1]
    else:
      changeIndex = nextArgs[0]
      frequency = nextArgs[1]
      visited = nextArgs[2]

echo firstRevisited(changes)
# Day 1: Chronal Calibration:7 ends here
