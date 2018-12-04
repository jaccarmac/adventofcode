import algorithm
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
  result = cmp(x.year, y.year)
  if result == 0:
    result = cmp(x.year, y.year)
  if result == 0:
    result = cmp(x.year, y.year)
  if result == 0:
    result = cmp(x.year, y.year)
  if result == 0:
    result = cmp(x.year, y.year)

var calendar = initTable[(int, int, int), (int, seq[int])]()
var guards = initTable[int, CountTable[int]]()
var currentGuard: int
var lastAsleep: int

echo records[0].kind

for record in records:
  let today = (record.year, record.month, record.day)
  case record.kind
  of rrDuty:
    echo "d"
    currentGuard = record.id
    echo currentGuard
    if not guards.contains currentGuard:
      guards[currentGuard] = initCountTable[int]()
    if not calendar.contains today:
      calendar[today] = (currentGuard, @[])
  of rrSleep:
    echo "s"
    lastAsleep = record.minute
  of rrWake:
    echo "w"
    for minute in lastAsleep.countup(record.minute - 1):
      if not guards[currentGuard].contains minute:
        guards[currentGuard][minute] = 1
      else:
        guards[currentGuard][minute].inc
      calendar[today][1].add minute
