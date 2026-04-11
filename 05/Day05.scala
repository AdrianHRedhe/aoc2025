package day05

import util.readInput

object Solution:
  case class IdRange(start: Long, stop: Long)

  def parse(input: List[String]): (List[IdRange], List[Long]) =
    def convertToIdRange(s: String): List[IdRange] =
      val pattern = """(\d+)-(\d+)""".r
      s match
        case "" => List()
        case pattern(start, stop) => List(IdRange(start.toLong, stop.toLong))

    val (l, r) = input.splitAt(input.indexOf("") + 1)
    (l.flatMap(convertToIdRange), r.map((s: String) => s.toLong))

  def inAnyRange(ranges: List[IdRange])(x: Long): Boolean =
    ranges.exists { case IdRange(start, stop) => (start <= x) && (x <= stop) }

  def part1(data: (List[IdRange], List[Long])): Long =
    val (ranges, ids) = data
    val isInAnyRange = inAnyRange(ranges)
    ids.count(isInAnyRange)

  def mergeRanges(ranges: List[IdRange]): List[IdRange] =
    ranges.sortBy(r => (r.start, r.stop)) match
      case Nil => List()
      case h :: t => 
        t.foldLeft(h :: Nil) {(acc, r) => 
          acc match
            case current :: rest if current.stop >= r.start =>
              IdRange(current.start, math.max(r.stop, current.stop)) :: rest
            case _ => r :: acc
      }

  def rangeLength(r: IdRange): Long =
    r.stop - r.start + 1

  def part2(data: (List[IdRange], List[Long])): Long =
    val (ranges, _) = data
    mergeRanges(ranges).map(rangeLength).sum

@main def main(): Unit =
  val input = readInput(5)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
