# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::*Day%204:%20Repose%20Record][Day 4: Repose Record:6]]
# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-4-problem-line][day-4-problem-line]]
type
  ReposeRecordKind = enum
    rrDuty
    rrWake
    rrSleep
  ReposeRecord = ref ReposeRecordObj
  ReposeRecordObj = object
    year: int
    month: int
    day: int
    hour: int
    minute: int
    case kind: ReposeRecordKind
    of rrDuty: id: int
    of rrWake, rrSleep: nil
  ProblemLine = ReposeRecord
# day-4-problem-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::read-problem-lines][read-problem-lines]]
import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
# read-problem-lines ends here
  # [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-4-parse-line][day-4-parse-line]]
  result = ReposeRecordObj.new
  let dateSplit = line.split ']'
  let dateTimeSplit = dateSplit[0][1..^1].split ' '
  let dateParts = dateTimeSplit[0].split '-'
  result.year = parseInt dateParts[0]
  result.month = parseInt dateParts[1]
  result.day = parseInt dateParts[2]
  let timeParts = dateTimeSplit[1].split ':'
  result.hour = parseInt timeParts[0]
  result.minute = parseInt timeParts[1]
  let recordParts = dateSplit[1][1..^1].split ' '
  case recordParts[0][0]
  of 'G':
    result.kind = rrDuty
    result.id = parseInt recordParts[1][1..^1]
  of 'f':
    result.kind = rrSleep
  of 'w':
    result.kind = rrWake
  else:
    raise newException(AssertionError, "invalid leading character")
  # day-4-parse-line ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-4-solution-1][day-4-solution-1]]
import algorithm
import math
import tables

let records = problem.sorted do (x, y: ReposeRecord) -> int:
  result = x.year.cmp y.year
  if result == 0:
    result = x.month.cmp y.month
  if result == 0:
    result = x.day.cmp y.day
  if result == 0:
    result = x.hour.cmp y.hour
  if result == 0:
    result = x.minute.cmp y.minute

var guards = initTable[int, CountTable[int]]()
var currentGuard: int
var lastAsleep: int

for record in records:
  case record.kind
  of rrDuty:
    currentGuard = record.id
    if not guards.contains currentGuard:
      guards[currentGuard] = initCountTable[int]()
  of rrSleep:
    lastAsleep = record.minute
  of rrWake:
    for minute in lastAsleep.countup(record.minute - 1):
      guards[currentGuard].inc minute

let sleepyGuards = toSeq(guards.pairs).sorted do (x, y: (int, CountTable[int])) -> int:
  toSeq(x[1].values).sum.cmp toSeq(y[1].values).sum

let sleepiestGuard = sleepyGuards[^1]

echo sleepiestGuard[0] * sleepiestGuard[1].largest[0]
# day-4-solution-1 ends here

# [[file:~/src/src/jaccarmac.com/adventofcode/2018/advent-of-nim.org::day-4-solution-2][day-4-solution-2]]
let consistentGuards = toSeq(guards.pairs).sorted do (x, y: (int, CountTable[int])) -> int:
  let xLargest = if x[1].len == 0: 0 else: x[1].largest[1]
  let yLargest = if y[1].len == 0: 0 else: y[1].largest[1]
  result = xLargest.cmp yLargest

let mostConsistentGuard = consistentGuards[^1]

echo mostConsistentGuard[0] * mostConsistentGuard[1].largest[0]
# day-4-solution-2 ends here
# Day 4: Repose Record:6 ends here
