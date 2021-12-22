type ProblemLine = tuple[x, y: int]

import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
  let coords = (line.split ',').map do (n: string) -> int: parseInt n.strip()
  (coords[0], coords[1])

func manhattan(x, y: (int, int)): int =
  abs(x[0] - y[0]) + abs(x[1] - y[1])

var minX = problem[0][0]
var maxX = problem[0][0]
var minY = problem[0][1]
var maxY = problem[0][1]

for coord in problem[1..^1]:
  minX = minX.min coord[0]
  maxX = maxX.max coord[0]
  minY = minY.min coord[1]
  maxY = maxY.max coord[1]

proc coordDistances(coord: (int, int)): seq[((int, int), int)] =
  problem.map do (c: (int, int)) -> ((int, int), int):
    (c, c.manhattan coord)

import algorithm
import options

proc closestProblemCoord(coord: (int, int)): Option[(int, int)] =
  let sortedDistances = coord.coordDistances().sorted do (x, y: ((int, int), int)) -> int:
    x[1].cmp y[1]
  if sortedDistances[0][1] != sortedDistances[1][1]:
    result = some sortedDistances[0][0]

import sets

var infiniteAreas = initSet[(int, int)]()

for x in countup(minX, maxX):
  for coord in @[(x, minY), (x, maxY)]:
    coord.closestProblemCoord().map do (input: (int, int)):
      infiniteAreas.incl input

for y in countup(minY, maxY):
  for coord in @[(minX, y), (maxX, y)]:
    coord.closestProblemCoord().map do (input: (int, int)):
      infiniteAreas.incl input

import tables

var areas = initCountTable[(int, int)]()

for x in countup(minX + 1, maxX - 1):
  for y in countup(minY + 1, maxY - 1):
    let coord = (x, y)
    coord.closestProblemCoord().map do (input: (int, int)):
      if not infiniteAreas.contains input:
        areas.inc input

areas.sort()

echo toSeq(areas.values)[0]

let maxDistance = if paramCount() > 1:
                    parseInt paramStr 2
                  else:
                    echo "max distance"
                    parseInt strip readAll stdin
var closeArea = 0

for x in countup(minX, maxX):
  for y in countup(minY, maxY):
    let coord = (x, y)
    let totalDistance = coord.coordDistances().foldl(a + b[1], 0)
    if totalDistance < maxDistance:
      inc closeArea

echo closeArea
