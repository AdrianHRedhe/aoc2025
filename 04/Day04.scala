package day04

import util.readInput

object Solution:
  case class Pos(x: Int, y: Int):
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)

  type Board = Map[Pos, Char]

  def parse(input: List[String]): Board =
    (for
      (row, y) <- input.zipWithIndex
      (cell, x) <- row.zipWithIndex
    yield Pos(x, y) -> cell).toMap

  val adjacentOffsets: IndexedSeq[Pos] =
    (for
      x <- -1 to 1
      y <- -1 to 1
    yield Pos(x,y)).filter(_ != Pos(0,0))

  def countAdjacentRolls(board: Board)(p: Pos): Int =
    adjacentOffsets.count(offset => board.get(p+offset).contains('@'))

  def rollPositions(board: Board): Set[Pos] =
    board.collect { case (pos, '@') => pos }.toSet

  def part1(board: Board): Int =
    rollPositions(board).filter(countAdjacentRolls(board)(_)<4).size

  def removeRollsRecursively(board: Board): Board =
    val toRemove = rollPositions(board).filter(countAdjacentRolls(board)(_)<4)

    if toRemove.isEmpty then board
    else removeRollsRecursively(board -- toRemove)

  def part2(board: Board): Int =
    val initial = rollPositions(board)
    val remaining = rollPositions(removeRollsRecursively(board))
    (initial -- remaining).size

@main def main(): Unit =
  val input = readInput(4)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
