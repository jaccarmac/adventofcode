# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%208:%20Memory%20Maneuver][Day 8: Memory Maneuver:5]]
# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::read-problem-stream][read-problem-stream]]
import os
import streams

var problem: proc(): Stream
if paramCount() > 0:
  problem = proc (): Stream =
    let problemFile = open paramStr 1
    newFileStream problemFile
else:
  let stdinString = readAll stdin
  problem = proc (): Stream =
    newStringStream stdinString
# read-problem-stream ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::problem-ints][problem-ints]]
import strutils

iterator problemInts(): int {.closure.} =
  let inStream = problem()
  var next = readChar inStream
  var num = ""
  while next != '\n':
    if next == ' ':
      yield parseInt num
      num = ""
    else:
      num &= next
    next = readChar inStream
  yield parseInt num
# problem-ints ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-8-solution-1][day-8-solution-1]]
var treeSource = problemInts
while true:
  let value = treeSource()
  if finished treeSource: break
  echo value
# day-8-solution-1 ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-8-solution-2][day-8-solution-2]]

# day-8-solution-2 ends here
# Day 8: Memory Maneuver:5 ends here
