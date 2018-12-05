# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%205:%20Alchemical%20Reduction.][Day 5: Alchemical Reduction.:4]]
# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::read-problem-stream][read-problem-stream]]
import os
import streams

let problem = (
  if paramCount() > 0: open paramStr 1 else: stdin
).newFileStream
# read-problem-stream ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-5-solution-1][day-5-solution-1]]
import deques
import strutils

var reagents = initDeque[char]()

reagents.addLast readChar problem
var nextReagent = readChar problem
while nextReagent != '\n':
  if reagents.len() > 0:
    let previousReagent = peekLast reagents
    if toLowerAscii(previousReagent) == toLowerAscii(nextReagent) and previousReagent != nextReagent:
      popLast reagents
    else:
      reagents.addLast nextReagent
  else:
    reagents.addLast nextReagent
  nextReagent = readChar problem

echo len reagents
# day-5-solution-1 ends here
# Day 5: Alchemical Reduction.:4 ends here
