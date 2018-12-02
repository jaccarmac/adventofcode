import intsets
import os
import sequtils
import strutils

let input = if paramCount() > 0: (readFile paramStr 1)[0..^2]
    else: readAll stdin

func parseChange(line: string): (char, int) =
    (line[0], parseInt line[1..^1])

let changes = map(splitLines input, parseChange)

func changeFrequency(start: int, change: (char, int)): int =
    case change[0]
        of '+':
            start + change[1]
        of '-':
            start - change[1]
        else:
            raise newException(AssertionError, "invalid leading character")

echo foldl(changes, changeFrequency(a, b), 0)

var frequency = 0
var visited = initIntSet()
block secondStar:
    while true:
        for change in changes:
            incl(visited, frequency)
            frequency = changeFrequency(frequency, change)
            if contains(visited, frequency):
                break secondStar
echo frequency
