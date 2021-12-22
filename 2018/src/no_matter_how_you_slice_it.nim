# [[file:../advent-of-nim.org::*Day 3: No Matter How You Slice It][Day 3: No Matter How You Slice It:6]]
# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-3-problem-line][day-3-problem-line]]][day-3-problem-line]]
type
  Claim = tuple[id, x, y, width, height: int]
  ProblemLine = Claim
# day-3-problem-line ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::read-problem-lines][read-problem-lines]]][read-problem-lines]]
import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
# read-problem-lines ends here
  # [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-3-parse-line][day-3-parse-line]]][day-3-parse-line]]
  let segments = splitWhitespace line
  result.id = parseInt segments[0][1..^1]
  let coords = split(segments[2], ",")
  result.x = parseInt coords[0]
  result.y = parseInt coords[1][0..^2]
  let dims = split(segments[3], "x")
  result.width = parseInt dims[0]
  result.height = parseInt dims[1]
  # day-3-parse-line ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-3-solution-1][day-3-solution-1]]][day-3-solution-1]]
import sets
import tables

var fabric = initTable[(int, int), HashSet[int]]()

iterator coordsFor(claim: Claim): (int, int) =
  for x in countup(claim.x, claim.x + claim.width - 1):
    for y in countup(claim.y, claim.y + claim.height - 1):
      yield (x, y)

for claim in problem:
  for coord in coordsFor claim:
    let x = coord[0]
    let y = coord[1]
    if not hasKey(fabric, (x, y)): fabric[(x, y)] = toSet @[claim.id]
    else:
      incl fabric[(x, y)], claim.id

let contestedClaims =
  filter(toSeq values fabric) do (cs: HashSet[int]) -> bool: len(cs) > 1

echo len contestedClaims
# day-3-solution-1 ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-3-solution-2][day-3-solution-2]]][day-3-solution-2]]
var goodClaims = toSet map(problem) do (c: Claim) -> int: c.id

for contestedClaim in contestedClaims:
  excl goodClaims, contestedClaim

echo goodClaims
# day-3-solution-2 ends here
# Day 3: No Matter How You Slice It:6 ends here
