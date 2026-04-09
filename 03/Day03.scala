package day03

import util.readInput

object Solution:
  def parse(input: List[String]): List[List[Int]] =
    def explode(s: String): List[Int] = 
        s.toList.map(_.toString.toInt)
    input.map(explode)

  def findMaxNonLast(l: List[Int]): Int =
    l.take(l.length - 1).max

  def findMaxPairP1(l: List[Int]): Int =
    val first = findMaxNonLast(l)
    val firstIndex = l.indexOf(first)
    val second = l.drop(firstIndex+1).max
    first*10+second
  
  def part1(data: (List[List[Int]])): Int =
    data.map(findMaxPairP1).reduceLeft(_ + _)

  def remainderAfterElement[A](l: List[A], x: A): List[A] =
    l.drop(l.indexOf(x)+1)

  def getMaxJoltage(n: Int)(l: List[Int]): Long =
    def go(n:Int, remaining: List[Int]): String =
      if n <= 0 then ""
      else
        val candidates = remaining.take(remaining.length - (n-1))
        val next = candidates.max
        next.toString() + go(n-1, remainderAfterElement(remaining, next))

    go(n, l).toLong

  def part2(data: (List[List[Int]])): Long =
    val nSwitches = 12
    data.map(getMaxJoltage(nSwitches)).reduceLeft(_ + _)

@main def main(): Unit =
  val input = readInput(3)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
