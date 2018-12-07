# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%206:%20Chronal%20Coordinates][Day 6: Chronal Coordinates:6]]
# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-6-problem-line][day-6-problem-line]]
type ProblemLine = tuple[x, y: int]
# day-6-problem-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::read-problem-lines][read-problem-lines]]
import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
# read-problem-lines ends here
  # [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-6-parse-line][day-6-parse-line]]
  let coords = (line.split ',').map do (n: string) -> int: parseInt n.strip()
  (coords[0], coords[1])
  # day-6-parse-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::manhattan][manhattan]]
func manhattan(x, y: (int, int)): int =
  abs(x[0] - y[0]) + abs(x[1] - y[1])
# manhattan ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-6-solution-1][day-6-solution-1]]
import algorithm
import tables

var minX = problem[0][0]
var maxX = problem[0][0]
var minY = problem[0][1]
var maxY = problem[0][1]

for coord in problem[1..^1]:
  minX = minX.min coord[0]
  maxX = maxX.max coord[0]
  minY = minY.min coord[1]
  maxY = maxY.max coord[1]

var areas = initCountTable[(int, int)]()

for x in minX.countup maxX:
  for y in minY.countup maxY:
    let coord = (x, y)
    let distances = problem.map do (c: (int, int)) -> ((int, int), int):
      (c, c.manhattan coord)
    let sortedDistances = distances.sorted do (x, y: ((int, int), int)) -> int:
      x[1].cmp y[1]
    if sortedDistances[0][1] != sortedDistances[1][1] and
       sortedDistances[0][0][0] != minX and
       sortedDistances[0][0][0] != maxX and
       sortedDistances[0][0][1] != minY and
       sortedDistances[0][0][1] != maxY:
      if not areas.contains sortedDistances[0][0]:
        areas[sortedDistances[0][0]] = 1
      else:
        areas[sortedDistances[0][0]].inc

areas.sort()

echo minX
echo maxX
echo minY
echo maxY
echo areas
echo toSeq(areas.values)[0]
# day-6-solution-1 ends here
# Day 6: Chronal Coordinates:6 ends here
