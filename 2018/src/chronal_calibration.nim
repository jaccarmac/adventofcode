# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%201:%20Chronal%20Calibration][Day 1: Chronal Calibration:12]]
# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-1-problem-line][day-1-problem-line]]
type ProblemLine = (char, int)
# day-1-problem-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::read-problem-lines][read-problem-lines]]
import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
# read-problem-lines ends here
  # [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-1-parse-line][day-1-parse-line]]
  (line[0], parseInt line[1..^1])
  # day-1-parse-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::change-frequency][change-frequency]]
func changeFrequency(start: int, change: (char, int)): int =
  case change[0]
  of '+':
    start + change[1]
  of '-':
    start - change[1]
  else:
    raise newException(AssertionError, "invalid leading character")
# change-frequency ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-1-solution-1][day-1-solution-1]]
echo foldl(problem, changeFrequency(a, b), 0)
# day-1-solution-1 ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-1-solution-2][day-1-solution-2]]
import sets

proc firstRevisited(
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
# day-1-solution-2 ends here
# Day 1: Chronal Calibration:12 ends here
