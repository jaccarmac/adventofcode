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

import math
import sequtils

func metadataSum(tree: Node): int =
  sum(tree.metadata) + sum(tree.children.map metadataSum)

echo metadataSum root

func nodeValue(tree: Node): int =
  if len(tree.children) == 0:
    result = sum tree.metadata
  else:
    for nodeRef in tree.metadata:
      let nodeIdx = nodeRef - 1
      if nodeIdx < len(tree.children):
        result += nodeValue tree.children[nodeIdx]

echo nodeValue root
