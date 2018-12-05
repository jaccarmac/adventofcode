# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%205:%20Alchemical%20Reduction.][Day 5: Alchemical Reduction.:5]]
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

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-5-solution-1][day-5-solution-1]]
import deques
import strutils

func polymerLength(polymer: iterator(): char): int =
  var reagents = initDeque[char]()
  for nextReagent in polymer():
    if reagents.len() > 0:
      let previousReagent = peekLast reagents
      if toLowerAscii(previousReagent) == toLowerAscii(nextReagent) and previousReagent != nextReagent:
        popLast reagents
      else:
        reagents.addLast nextReagent
    else:
      reagents.addLast nextReagent
  len reagents

iterator solution1Polymer(): char {.closure.} =
  let inStream = problem()
  var nextReagent = readChar inStream
  while nextReagent != '\n':
    yield nextReagent
    nextReagent = readChar inStream

echo polymerLength(solution1Polymer)
# day-5-solution-1 ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-5-solution-2][day-5-solution-2]]
func solution2Polymer(exclude: char): iterator(): char {.closure.} =
  (iterator(): char {.closure.} =
     let inStream = problem()
     var nextReagent = readChar inStream
     while nextReagent != '\n':
       if toLowerAscii(nextReagent) != toLowerAscii(exclude):
         yield nextReagent
       nextReagent = readChar inStream)

var minLength = high(int)

for reagent in countup('a', 'z'):
  minLength = minLength.min polymerLength solution2Polymer reagent

echo minLength
# day-5-solution-2 ends here
# Day 5: Alchemical Reduction.:5 ends here
