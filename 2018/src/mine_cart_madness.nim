# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%2013:%20Mine%20Cart%20Madness][Day 13: Mine Cart Madness:5]]
# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-13-problem-line][day-13-problem-line]]
import tables

type
  TrackSegment = enum
    Verti
    Horiz
    DiagR
    DiagL
    Inter
  CartDirection = enum
    N
    NE
    E
    SE
    S
    SW
    W
    NW
  ProblemLine = bool

var startingCarts: seq[((int, int), CartDirection)]
var track = initTable[(int, int), TrackSegment]()

var row = 0
# day-13-problem-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::read-problem-lines][read-problem-lines]]
import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
# read-problem-lines ends here
  # [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-13-parse-line][day-13-parse-line]]
  var col = 0
  
  for c in line:
    case c
    of '|':
      track[(col, row)] = Verti
    of '-':
      track[(col, row)] = Horiz
    of '/':
      track[(col, row)] = DiagR
    of '\\':
      track[(col, row)] = DiagL
    of '+':
      track[(col, row)] = Inter
    of '^':
      track[(col, row)] = Verti
      startingCarts.add ((col, row), N)
    of 'v':
      track[(col, row)] = Verti
      startingCarts.add ((col, row), S)
    of '>':
      track[(col, row)] = Horiz
      startingCarts.add ((col, row), E)
    of '<':
      track[(col, row)] = Horiz
      startingCarts.add ((col, row), W)
    else: discard
    inc col
  
  inc row
  # day-13-parse-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-13-solution-1][day-13-solution-1]]
echo startingCarts
echo track
# day-13-solution-1 ends here
# Day 13: Mine Cart Madness:5 ends here
