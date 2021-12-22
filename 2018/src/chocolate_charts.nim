# [[file:../advent-of-nim.org::*Day 14: Chocolate Charts][Day 14: Chocolate Charts:5]]
# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-14-problem][day-14-problem]]][day-14-problem]]
import os

let problem = paramStr 1
# day-14-problem ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-14-solution-1][day-14-solution-1]]][day-14-solution-1]]
import sequtils
import strutils

proc combine(recipes: var seq[int], elf1Current, elf2Current: int) =
  let newRecipes = recipes[elf1Current] + recipes[elf2Current]
  let digits = ($newRecipes).map(func (c: char): string = $c).map parseInt
  for digit in digits:
    recipes.add digit


func solution1(recipesToTry: int): string =
  var recipes = @[3, 7]
  var elf1Current = 0
  var elf2Current = 1
  while recipesToTry + 10 >= len recipes:
    recipes.combine elf1Current, elf2Current
    elf1Current += recipes[elf1Current] + 1
    elf2Current += recipes[elf2Current] + 1
    elf1Current = elf1Current mod len recipes
    elf2Current = elf2Current mod len recipes
  recipes[recipesToTry..recipesToTry+9].map(func (s: int): string = $s).join ""

echo solution1 parseInt problem
# day-14-solution-1 ends here

# [[[[file:~/src/adventofcode/2018/advent-of-nim.org::day-14-solution-2][day-14-solution-2]]][day-14-solution-2]]
func solution2(scoresToSearch: seq[int]): int =
  var recipes = @[3, 7]
  var elf1Current = 0
  var elf2Current = 1
  while true:
    recipes.combine elf1Current, elf2Current
    elf1Current += recipes[elf1Current] + 1
    elf2Current += recipes[elf2Current] + 1
    elf1Current = elf1Current mod len recipes
    elf2Current = elf2Current mod len recipes
    if len(scoresToSearch) <= len(recipes):
      if len(scoresToSearch) < len(recipes) and scoresToSearch == recipes[^(len(scoresToSearch)+1)..^2]:
        return len(recipes) - len(scoresToSearch) - 1
      if scoresToSearch == recipes[^len(scoresToSearch)..^1]:
        return len(recipes) - len(scoresToSearch)

echo solution2 problem.map(func (c: char): int = parseInt $c)
# day-14-solution-2 ends here
# Day 14: Chocolate Charts:5 ends here
