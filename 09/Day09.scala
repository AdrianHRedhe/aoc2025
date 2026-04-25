package day09

import util.readInput

object Solution:
  case class Pos(x: Long, y: Long):
    def area(other: Pos): Long = 
      // squares allow one side to be 0
      (math.abs(x - other.x) + 1) * (math.abs(y-other.y) + 1)

  def parse(input: List[String]): List[Pos] =
    input.map {s => s.split(",") match
      case Array(x, y) => Pos(x.toLong, y.toLong)
    }

  def part1(boundaryNodes: List[Pos]): Long =
    boundaryNodes.combinations(2).collect {case List(n1, n2) => n1.area(n2)}.max

  case class Edge(min: Pos, max: Pos)
  case class Rectangle(minX: Long, minY: Long, maxX: Long, maxY: Long)

  def sortEdge(n1: Pos, n2: Pos): Edge =
    Edge(
      min = Pos(math.min(n1.x, n2.x), math.min(n1.y, n2.y)),
      max = Pos(math.max(n1.x, n2.x), math.max(n1.y, n2.y))
    )

  def getEdges(boundaryNodes: List[Pos]): Set[Edge] =
    val rotated = boundaryNodes.tail :+ boundaryNodes.head
    boundaryNodes.zip(rotated).map { case (n1, n2) => sortEdge(n1, n2) }.toSet

  def edgeIntersectsRectangle(rectangle: Rectangle)(edge: Edge): Boolean =
    val Edge(Pos(eMinX, eMinY), Pos(eMaxX, eMaxY)) = edge
    val Rectangle(rMinX, rMinY, rMaxX, rMaxY) = rectangle
    // Defining these as functions means that they will be calculated once, upon activation
    // they will also not calculate if they are shortcircuited by early exit
    def isHorizontalEdge = eMinY == eMaxY
    def isVerticalEdge = eMinX == eMaxX
    
    def edgeInRectangleYSpan = rMinY < eMinY && eMinY < rMaxY
    def edgeInRectangleXSpan = rMinX < eMinX && eMinX < rMaxX
    
    def rectangleLeftEdgeInEdgeXSpan = eMinX <= rMinX && rMinX < eMaxX
    def rectangleRightEdgeInEdgeXSpan = eMinX < rMaxX && rMaxX <= eMaxX

    def rectangleTopEdgeInEdgeYSpan = eMinY <= rMinY && rMinY < eMaxY
    def rectangleBottomEdgeInEdgeYSpan = eMinY < rMaxY && rMaxY <= eMaxY

    def horizontalEdgeCutsThroughRectangle = 
      isHorizontalEdge 
      && edgeInRectangleYSpan 
      && (rectangleLeftEdgeInEdgeXSpan || rectangleRightEdgeInEdgeXSpan)
    
    def verticalEdgeCutsThroughRectangle = 
      isVerticalEdge 
      && edgeInRectangleXSpan 
      && (rectangleTopEdgeInEdgeYSpan || rectangleBottomEdgeInEdgeYSpan)
    
    horizontalEdgeCutsThroughRectangle || verticalEdgeCutsThroughRectangle

  def hasIntersection(edges: Set[Edge])(n1: Pos, n2: Pos): Boolean = 
    val (rMinX, rMaxX) = if n1.x <= n2.x then (n1.x, n2.x) else (n2.x, n1.x)
    val (rMinY, rMaxY) = if n1.y <= n2.y then (n1.y, n2.y) else (n2.y, n1.y)
    val rectangle = Rectangle(rMinX, rMinY, rMaxX, rMaxY)
    val edgeIntersectsThisRectangle = edgeIntersectsRectangle(rectangle)
    edges.exists(edge => edgeIntersectsThisRectangle(edge))

  def part2(boundaryNodes: List[Pos]): Long = 
    val sortedEdges = getEdges(boundaryNodes)
    val hasEdgeIntersection = hasIntersection(sortedEdges)
    val validCombinations = boundaryNodes.combinations(2).filter {
      case List(n1, n2) => !hasEdgeIntersection(n1, n2)
      case _ => false
    }
    validCombinations.collect {case List(n1, n2) => n1.area(n2)}.max

@main def main(): Unit =
  val input = readInput(9)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
