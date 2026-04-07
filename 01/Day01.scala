package day01

import util.readInput

object Solution:
  def parse(input: List[String]): List[Int] =
    def parseSingle(s: String): Int = 
      s.head match
        case 'R' => s.tail.toInt
        case _ => -s.tail.toInt
    input.map(parseSingle)

  def updatePosition(pos: Int, action: Int): Int =
    val nextPos = (pos + action) % 100
    // Modulo gives +/- answers up to 100 in scala
    if nextPos >= 0 then nextPos else 100 + nextPos

  def allPositions(initPos: Int, input: List[Int]): List[Int] =
    input.scanLeft(initPos)(updatePosition)
  
  def part1(data: (List[Int])): Int =
    val initPos = 50
    val positions = allPositions(initPos, data)
    val zeroPositions = positions.filter(_ == 0)
    zeroPositions.length

  def explodeAllTicks(movement: Int): List[Int] =
    // for -5 we would expand to [-1, -1, -1, -1, -1]
    List.fill(movement.abs)(movement.sign)

  def part2(data: List[Int]): Int =
    val initPos = 50
    val allTicks = data.flatMap(explodeAllTicks)
    val zeroPositions = 
      allTicks
      .scanLeft(initPos)(_ + _)
      .filter(_ % 100 == 0)
    zeroPositions.length

@main def main(): Unit =
  val input = readInput(1)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
