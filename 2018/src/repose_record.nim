import algorithm
import math
import os
import sequtils
import strutils
import tables

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

let unsortedRecords = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ReposeRecord:
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

let records = unsortedRecords.sorted do (x, y: ReposeRecord) -> int:
  result = x.year.cmp y.year
  if result == 0:
    result = x.month.cmp y.month
  if result == 0:
    result = x.day.cmp y.day
  if result == 0:
    result = x.hour.cmp y.hour
  if result == 0:
    result = x.minute.cmp y.minute

var calendar = initTable[(int, int, int), seq[int]]()
var guards = initTable[int, CountTable[int]]()
var currentGuard: int
var lastAsleep: int

for record in records:
  let today = (record.year, record.month, record.day)
  if not calendar.contains today:
      calendar[today] = @[]
  case record.kind
  of rrDuty:
    currentGuard = record.id
    if not guards.contains currentGuard:
      guards[currentGuard] = initCountTable[int]()
  of rrSleep:
    lastAsleep = record.minute
  of rrWake:
    for minute in lastAsleep.countup(record.minute - 1):
      if not guards[currentGuard].contains minute:
        guards[currentGuard][minute] = 1
      else:
        guards[currentGuard][minute].inc
      calendar[today].add minute

let bestGuards = toSeq(guards.pairs).sorted do (x, y: (int, CountTable[int])) -> int:
  toSeq(x[1].values).sum.cmp toSeq(y[1].values).sum

let bestGuard = bestGuards[^1]

echo bestGuard[0] * bestGuard[1].largest[0]
