package day12

import util.readInput

object Solution:
  case class Pos(x: Int, y: Int)
  type Board = Map[Pos, Int]
  type Pkg = Set[Pos]
  case class Region(x: Int, y: Int, packages: List[Int])

  def parse(input: List[String]): (List[Pkg], List[Region]) =
    def toPackage(s: String): Pkg =
      val lines = s.tail.split("\n")
      (for
        (line, x) <- lines.zipWithIndex
        (char, y) <- line.zipWithIndex if char == '#'
      yield Pos(x,y)).toSet
    
    def toRegion(s: String): Region = 
      val Array(xy, packageString) = s.split(": ")
      val Array(x, y) = xy.split("x")
      Region(x.toInt, y.toInt, packageString.split(" ").map(_.toInt).toList)

    val sublists = input.mkString("\n").split("\n\n").toList
    val packages = sublists.init.map(toPackage)
    val regions = sublists.last.split("\n").map(toRegion).toList
    (packages, regions)

  def getBoard(x: Int, y: Int): Board = 
    (for 
      w <- 0 to x
      h <- 0 to y
    yield Pos(x, y) -> 0).toMap

  def earlyFail(packages: List[Pkg], region: Region): Boolean =
    if (region.x * region.y) < packages.flatten.length then true
    else false

  def earlySuccess(packages: List[Pkg], region: Region): Boolean =
    // if we were to make each box a 3x3 and then shrink rectangle to be modulo 3 then we
    // can be sure that we fit inside the box since each polymino is within 3x3 bounds
    val effectiveX = (region.x/3)*3
    val effectiveY = (region.y/3)*3
    if (effectiveX * effectiveY) >= packages.length * 9 then true
    else false

  def canFitPackages(packages: List[Pkg])(region: Region): Boolean =
    val packagesInRegion = region.packages.zipWithIndex.flatMap(
      (count, idx) => List.fill(count)(packages(idx))
    )

    if earlyFail(packagesInRegion, region) then false
    else if earlySuccess(packagesInRegion, region) then true
    else
      // Todo: actual polymino packing logic after quick checks
      false 

  def part1(packages: List[Pkg], regions: List[Region]): Long =
    regions.count(canFitPackages(packages))

@main def main(): Unit =
  val input = readInput(12)
  val (packages, regions) = Solution.parse(input)

  val part1 = Solution.part1(packages, regions)

  println(s"Part 1: $part1")
