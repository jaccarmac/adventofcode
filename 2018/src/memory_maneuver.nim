# [[file:../advent-of-nim.org::*Day 8: Memory Maneuver][Day 8: Memory Maneuver:6]]
# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::read-problem-stream][read-problem-stream]]][read-problem-stream]]
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

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::problem-ints][problem-ints]]][problem-ints]]
import strutils

iterator problemInts(): int {.closure.} =
  let inStream = problem()
  var next = readChar inStream
  var num = ""
  while next != '\n':
    if next == ' ':
      yield parseInt num
      num = ""
    else:
      num &= next
    next = readChar inStream
  yield parseInt num
# problem-ints ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-8-build-tree][day-8-build-tree]]][day-8-build-tree]]
type
  Header = tuple[children, metadata: int]
  Node = ref object
    header: Header
    children: seq[Node]
    metadata: seq[int]

var treeSource = problemInts

proc buildTree(source: iterator(): int): Node =
  new result
  result.header.children = treeSource()
  result.header.metadata = treeSource()
  for _ in 1.countup result.header.children:
    result.children.add buildTree source
  for _ in 1.countup result.header.metadata:
    result.metadata.add treeSource()

var root = buildTree treeSource
# day-8-build-tree ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-8-solution-1][day-8-solution-1]]][day-8-solution-1]]
import math
import sequtils

func metadataSum(tree: Node): int =
  sum(tree.metadata) + sum(tree.children.map metadataSum)

echo metadataSum root
# day-8-solution-1 ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-8-solution-2][day-8-solution-2]]][day-8-solution-2]]
func nodeValue(tree: Node): int =
  if len(tree.children) == 0:
    result = sum tree.metadata
  else:
    for nodeRef in tree.metadata:
      let nodeIdx = nodeRef - 1
      if nodeIdx < len(tree.children):
        result += nodeValue tree.children[nodeIdx]

echo nodeValue root
# day-8-solution-2 ends here
# Day 8: Memory Maneuver:6 ends here
