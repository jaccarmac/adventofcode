# [[file:../advent-of-nim.org::*Day 13: Mine Cart Madness][Day 13: Mine Cart Madness:7]]
# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-13-problem-line][day-13-problem-line]]][day-13-problem-line]]
import tables

type
  TrackSegment = enum
    Verti
    Horiz
    DiagR
    DiagL
    Inter
  CartDirection = enum
    N
    E
    S
    W
  Coord = tuple[col, row: int]
  Track = Table[Coord, TrackSegment]
  TurnDirection = enum
    Left
    Right
    Straight
  CartSimulation = tuple
    loc: Coord
    dir: CartDirection
    lastTurn: TurnDirection
    alive: bool
  TrackSimulation = tuple
    track: Track
    carts: seq[CartSimulation]
  ProblemLine = bool

var startingCarts: seq[(Coord, CartDirection)]
var track = initTable[Coord, TrackSegment]()

var row = 0
# day-13-problem-line ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::read-problem-lines][read-problem-lines]]][read-problem-lines]]
import os
import sequtils
import strutils

let problem = (
  if paramCount() > 0: readFile paramStr 1 else: readAll stdin
)[0..^2].splitLines.map do (line: string) -> ProblemLine:
# read-problem-lines ends here
  # [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-13-parse-line][day-13-parse-line]]][day-13-parse-line]]
  var col = 0
  
  for c in line:
    case c
    of '|':
      track[(col, row)] = Verti
    of '-':
      track[(col, row)] = Horiz
    of '/':
      track[(col, row)] = DiagR
    of '\\':
      track[(col, row)] = DiagL
    of '+':
      track[(col, row)] = Inter
    of '^':
      track[(col, row)] = Verti
      startingCarts.add ((col, row), N)
    of 'v':
      track[(col, row)] = Verti
      startingCarts.add ((col, row), S)
    of '>':
      track[(col, row)] = Horiz
      startingCarts.add ((col, row), E)
    of '<':
      track[(col, row)] = Horiz
      startingCarts.add ((col, row), W)
    else: discard
    inc col
  
  inc row
  # day-13-parse-line ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-13-simulation-funcs][day-13-simulation-funcs]]][day-13-simulation-funcs]]
import algorithm

proc sort(carts: var seq[CartSimulation]) =
  carts.sort do (x, y: CartSimulation) -> int:
    result = x.loc.row.cmp y.loc.row
    if result == 0:
      result = x.loc.col.cmp y.loc.col

func nextLocation(cart: CartSimulation): Coord =
  case cart.dir
  of N: (cart.loc.col, cart.loc.row - 1)
  of E: (cart.loc.col + 1, cart.loc.row)
  of S: (cart.loc.col, cart.loc.row + 1)
  of W: (cart.loc.col - 1, cart.loc.row)

proc turn(cart: var CartSimulation): TurnDirection =
  case cart.lastTurn
  of Left:
    result = Straight
  of Straight:
    result = Right
  of Right:
    result = Left
  cart.lastTurn = result

proc move(track: Track, cart: var CartSimulation) =
  let endCoord = nextLocation cart
  let newDirection = case cart.dir
                     of N:
                       case track[endCoord]
                       of Verti: N
                       of DiagR: E
                       of DiagL: W
                       of Inter:
                         case turn cart
                         of Left: W
                         of Straight: N
                         of Right: E
                       else: raise newException(AssertionError, "bad turn")
                     of E:
                       case track[endCoord]
                       of Horiz: E
                       of DiagR: N
                       of DiagL: S
                       of Inter:
                         case turn cart
                         of Left: N
                         of Straight: E
                         of Right: S
                       else: raise newException(AssertionError, "bad turn")
                     of S:
                       case track[endCoord]
                       of Verti: S
                       of DiagR: W
                       of DiagL: E
                       of Inter:
                         case turn cart
                         of Left: E
                         of Straight: S
                         of Right: W
                       else: raise newException(AssertionError, "unreachable")
                     of W:
                       case track[endCoord]
                       of Horiz: W
                       of DiagR: S
                       of DiagL: N
                       of Inter:
                         case turn cart
                         of Left: S
                         of Straight: W
                         of Right: N
                       else: raise newException(AssertionError, "bad turn")
  cart.loc = endCoord
  cart.dir = newDirection
# day-13-simulation-funcs ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-13-solution-1][day-13-solution-1]]][day-13-solution-1]]
proc solution1() =
  var solution1Sim: TrackSimulation = (track, @[])
  for cart in startingCarts:
    solution1Sim.carts.add (cart[0], cart[1], Right, true)
  while true:
    sort solution1Sim.carts
    for i in 0 ..< solution1Sim.carts.len:
      solution1Sim.track.move solution1Sim.carts[i]
      let crash = solution1Sim.carts.filter do (c: CartSimulation) -> bool:
        c.loc == solution1Sim.carts[i].loc
      if len(crash) > 1:
        echo crash[0].loc
        return

solution1()
# day-13-solution-1 ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-13-solution-2][day-13-solution-2]]][day-13-solution-2]]
import sets

proc solution2() =
  var sim: TrackSimulation = (track, @[])
  for cart in startingCarts:
    sim.carts.add (cart[0], cart[1], Right, true)
  while true:
    sort sim.carts
    for i in 0 ..< sim.carts.len:
      if sim.carts[i].alive:
        sim.track.move sim.carts[i]
        let crash = sim.carts.filter do (c: CartSimulation) -> bool:
          c.alive and c.loc == sim.carts[i].loc
        if len(crash) > 1:
          var removed = 0
          for j in 0 ..< sim.carts.len:
            if sim.carts[j].loc == crash[0].loc and sim.carts[j].alive and removed < 2:
              inc removed
              sim.carts[j].alive = false
    var alive = sim.carts.filter do (c: CartSimulation) -> bool:
      c.alive
    if len(alive) == 1:
      echo alive[0].loc
      return

solution2()
# day-13-solution-2 ends here
# Day 13: Mine Cart Madness:7 ends here
