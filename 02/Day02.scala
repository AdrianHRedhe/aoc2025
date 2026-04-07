package day02

import util.readInput

object Solution:
  case class IdRange(start: Long, stop: Long)

  def parse(input: List[String]): List[IdRange] =
    def explode(s: String): List[String] = 
      s.split(',').toList

    def convertToIdRange(s: String): IdRange =
      val pattern = """(\d+)-(\d+)""".r
      s match
        case pattern(start, stop) => IdRange(start.toLong, stop.toLong)
    input.flatMap(explode).map(convertToIdRange)

  def hasInvalidIdP1(l: Long): Boolean =
    val s = l.toString
    val mid = s.length / 2
    val left = s.take(mid)
    val right = s.drop(mid)
    left == right

  def getFromRangeWhere(p: Long => Boolean)(r: IdRange): List[Long] =
    (r.start to r.stop + 1).filter(p).toList

  def part1(data: (List[IdRange])): Long =
    data.flatMap(getFromRangeWhere(hasInvalidIdP1)).reduceLeft(_ + _)

  def hasInvalidIdP2(l: Long): Boolean =
    val s = l.toString
    val half = s.length / 2
    (1 to half).exists(len =>
      // we only need to check if x * len of sub is == len of s
      s.length % len == 0 && isRepeating(s)(s.take(len))
    )

  def isRepeating(s: String)(sub: String): Boolean =
    val mult = s.length / sub.length
    sub.repeat(mult) == s

  def part2(data: (List[IdRange])): Long =
    data.flatMap(getFromRangeWhere(hasInvalidIdP2)).reduceLeft(_ + _)

@main def main(): Unit =
  val input = readInput(2)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
