package day06

import util.readInput

object Solution:
  case class Homework(longs: List[Long], op: String)

  def parseP1(input: List[String]): List[Homework] =
    def parseLongs(s: String): List[Long] =
      val pattern = "\\s+"
      s.split(pattern).filter(!_.isEmpty).map(_.toLong).toList

    val (numberLines, operatorLines) = input.splitAt(input.length-1)
    val operatorLine = operatorLines.head

    val hwLongs = numberLines.map(parseLongs).transpose
    val ops = operatorLine.split("\\s+")
    hwLongs.zip(ops).map(Homework(_, _))

  def calculateHomework(hw: Homework): Long =
    val op: (Long, Long) => Long = if hw.op == "+" then (_+_) else (_*_)
    hw.longs.reduce(op)

  def part1(dataP1: (List[Homework])): Long =
    dataP1.map(calculateHomework).sum

  def operatorIndexes(s: String): List[Int] =
    s.zipWithIndex.collect {
      case ('+' | '*', idx) => idx
    }.toList

  def parseP2(input: List[String]): List[Homework] =
    def extractColumnChars(bounds: List[Int])(s: String): List[List[Char]] =
      bounds.sliding(2).map {
        case Seq(start, stop) => s.slice(start, stop).toList
        case _ => List()
      }.toList

    def columnsToTransposedLongs(cs: List[List[Char]]): List[Long] =
      // want to turn 
      // ((' ','1','2'),
      //  ('3','4','5'),
      //  ('6','7',' ')),
      // to (36, 147, 25)
      cs.transpose.map(_.mkString.replace(" ", "")).filter(!_.isEmpty).map(_.toLong)

    val (numberLines, operatorLines) = input.splitAt(input.length-1)
    val operatorLine = operatorLines.head

    val columnBounds = operatorIndexes(operatorLine) :+ operatorLine.length
    val getColumnChars = extractColumnChars(columnBounds)

    val columns = numberLines.map(getColumnChars).transpose
    val hwLongs = columns.map(columnsToTransposedLongs)
    val ops = operatorLine.split("\\s+")
    hwLongs.zip(ops).map(Homework(_, _))

  def part2(dataP2: List[Homework]): Long =
    dataP2.map(calculateHomework).sum

@main def main(): Unit =
  val input = readInput(6)
  val dataP1 = Solution.parseP1(input)
  val dataP2 = Solution.parseP2(input)

  val part1 = Solution.part1(dataP1)
  val part2 = Solution.part2(dataP2)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
