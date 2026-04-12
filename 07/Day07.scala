package day07

import util.readInput
import scala.annotation.tailrec

object Solution:
  case class Pos(x: Int, y: Int):
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
    def down: Pos = this + Pos(0, 1)
    def left: Pos = this + Pos(-1, 0)
    def right: Pos = this + Pos(1, 0)

  type Board = Map[Pos, Char]
  type Cache = Map[Pos, Long]

  def parse(input: List[String]): Board =
    (for
      (row, y) <- input.zipWithIndex
      (cell, x) <- row.zipWithIndex
    yield Pos(x, y) -> cell).toMap

  def startPosition(board: Board): Pos =
    board.collectFirst { case (pos, 'S') => pos }.get

  def countUniqueSplits(board: Board): Int =
    @annotation.tailrec
    def loop(board: Board, positionsToUpdate: List[Pos], nSplits: Int): Int =
      if positionsToUpdate.isEmpty then nSplits
      else
        val nextPositions = positionsToUpdate.flatMap(p => 
          board.get(p.down) match
            case Some('.') => List(p.down)
            case Some('^') => List(p.down.left, p.down.right)
            case _ => Nil
        ).distinct
        val splits = positionsToUpdate.count(p =>
            board.get(p.down).contains('^')
        )
        loop(board, nextPositions, nSplits + splits)

    loop(board, List(startPosition(board)), 0)

  def part1(board: Board): Int =
    countUniqueSplits(board)

  def countTotalSplits(board: Board): Long =
    def countFromPos(pos: Pos, cache: Cache): (Long, Cache) =
      cache.get(pos) match
        case Some(result) => (result, cache)
        case None =>
          val down = pos.down
          val (result, newCache) = board.get(down) match
            case Some('.') => 
              countFromPos(down, cache)
            case Some('^') =>
              val left = down.left
              val right = down.right
              val (leftResult, cache1) = countFromPos(left, cache)
              val (rightResult, cache2) = countFromPos(right, cache1)
              (1 + leftResult + rightResult, cache2)
            case _ =>
              (0L, cache)
          (result, newCache + (pos -> result))
    
    countFromPos(startPosition(board), Map())._1

  def part2(board: Board): Long = 
    // Total beams = initial beam + beams created by splits
    1 + countTotalSplits(board)

@main def main(): Unit =
  val input = readInput(7)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
