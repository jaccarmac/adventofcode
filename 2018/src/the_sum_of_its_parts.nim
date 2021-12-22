type ProblemLine = tuple[step, dependsOn: char]

import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
  let words = line.split ' '
  (words[7][0], words[1][0])

import algorithm
import options
import tables
import sets

var dependencies = initTable[char, HashSet[char]]()

for dependency in problem:
  if not dependencies.contains dependency.step:
    dependencies[dependency.step] = initSet[char]()
  if not dependencies.contains dependency.dependsOn:
    dependencies[dependency.dependsOn] = initSet[char]()
  dependencies[dependency.step].incl dependency.dependsOn

func nextStep(dependencies: Table): Option[char] =
  let validSteps = toSeq(dependencies.pairs).filter do (pair: (char, HashSet[char])) -> bool:
    len(pair[1]) == 0
  let sortedSteps = validSteps.sorted do (x, y: (char, HashSet[char])) -> int:
    x[0].cmp y[0]
  if len(sortedSteps) > 0:
    result = some(sortedSteps[0][0])

var instructions = ""

while len(dependencies) > 0:
  let step = get nextStep dependencies
  instructions &= step
  dependencies.del step
  for k in dependencies.keys:
    dependencies[k].excl step

echo instructions

let elfCount = if paramCount() > 1:
                 parseInt paramStr 2
               else:
                 echo "elf count"
                 parseInt strip readAll stdin

let timeForA = if paramCount() > 2:
                 parseInt paramStr 3
               else:
                 echo "time to perform step A"
                 parseInt strip readAll stdin

for dependency in problem:
  if not dependencies.contains dependency.step:
    dependencies[dependency.step] = initSet[char]()
  if not dependencies.contains dependency.dependsOn:
    dependencies[dependency.dependsOn] = initSet[char]()
  dependencies[dependency.step].incl dependency.dependsOn

var time = 0
var elves: seq[(char, int)] = @[]

func timeFor(timeForA: int, step: char): int =
  timeForA + (ord(step) - ord('A'))

while len(dependencies) + len(elves) > 0:
  for i, elf in elves:
    if elf[1] == time:
      let step = elf[0]
      for k in dependencies.keys:
        dependencies[k].excl step
      elves.del i
  if len(elves) < elfCount:
    let step = nextStep dependencies
    if isSome step:
      elves.add (get step, time + timeForA.timeFor get step)
      dependencies.del get step
      continue
  inc time

echo time - 1
