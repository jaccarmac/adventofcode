import os
import sequtils
import sets
import strutils
import tables

type Claim = tuple[id, x, y, width, height: int]

let claims = map(splitLines(
  if paramCount() > 0: (readFile paramStr 1)
  else: readAll stdin
)[0..^2]) do (line: string) -> Claim:
  let segments = splitWhitespace line
  result.id = parseInt segments[0][1..^1]
  let coords = split(segments[2], ",")
  result.x = parseInt coords[0]
  result.y = parseInt coords[1][0..^2]
  let dims = split(segments[3], "x")
  result.width = parseInt dims[0]
  result.height = parseInt dims[1]

var fabric = initTable[(int, int), HashSet[int]]()

iterator coordsFor(claim: Claim): (int, int) =
  for x in countup(claim.x, claim.x + claim.width - 1):
    for y in countup(claim.y, claim.y + claim.height - 1):
      yield (x, y)

for claim in claims:
  for coord in coordsFor claim:
    let x = coord[0]
    let y = coord[1]
    if not hasKey(fabric, (x, y)): fabric[(x, y)] = toSet @[claim.id]
    else:
      incl fabric[(x, y)], claim.id

let contestedClaims =
  filter(toSeq values fabric) do (cs: HashSet[int]) -> bool: len(cs) > 1

echo len contestedClaims

var goodClaims = toSet map(claims) do (c: Claim) -> int: c.id

for contestedClaim in contestedClaims:
  excl goodClaims, contestedClaim

echo goodClaims
