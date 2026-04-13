package day08

import util.readInput
import scala.annotation.tailrec

object Solution:
  case class Node(x: Int, y: Int, z:Int):
    def dist(other: Node): Double = 
      math.sqrt(
        math.pow(x - other.x, 2)
        + math.pow(y - other.y, 2)
        + math.pow(z - other.z, 2)
      ) 

  case class Edge(n1: Node, n2: Node):
    // Use commutative operations for equality to be able to add to cache once
    override def equals(obj: Any): Boolean = obj match {
      case Edge(a, b) => 
        (n1 == a && n2 == b) || (n1 == b && n2 == a)
      case _ => false
    }
    
    override def hashCode(): Int = n1.hashCode + n2.hashCode

    def isValid: Boolean = n1 != n2
    
    def length: Double = n1.dist(n2)

  type Cache = Map[Edge, Double]

  case class Circuit(nodes: List[Node]):
    def contains(other: Node): Boolean = nodes.contains(other)
    def contains(other: Edge): Boolean = contains(other.n1) && contains(other.n2)
    def +(other: Circuit): Circuit = Circuit((nodes.toSet ++ other.nodes.toSet).toList)


  def parse(input: List[String]): List[Node] =
    input.map {s => s.split(",") match
      case Array(x, y, z) => Node(x.toInt, y.toInt, z.toInt) 
    }

  def cacheDistances(node: Node, others: List[Node], cache: Cache): Cache =
    val newEdges = others.map(Edge(node, _)).filter(e => e.isValid && !cache.contains(e))
    cache ++ newEdges.map(e => e -> e.length)
    
  def cacheAllDistances(nodes: List[Node]): Cache = 
    nodes.foldLeft(Map.empty[Edge, Double])((acc, n) => cacheDistances(n, nodes, acc))

  def updateCircuits(edge: Edge, circuits: Set[Circuit]): Set[Circuit] =
    val toRemove = circuits.filter(c => c.contains(edge.n1) || c.contains(edge.n2))
    val toAdd = toRemove.reduce(_+_)
    circuits -- toRemove + toAdd

  def updateCircuitsUpTo(n: Int, edges: Seq[Edge], nodes: List[Node]): Int =
    @tailrec
    def go(n: Int, edges: Seq[Edge], circuits: Set[Circuit]): Set[Circuit] = 
      (n, edges) match
        // If no more edges to examine or no more iterations to run through
        case (0, _) | (_, Seq()) => circuits
        case (_, head :: tail) =>
          val updatedCircuits = 
            if circuits.exists {circuit => circuit.contains(head)} then circuits
            else updateCircuits(head, circuits)
          go(n-1, tail, updatedCircuits)

    val circuits = nodes.map(n => Circuit(List(n))).toSet
    val finalCircuits = go(n, edges, circuits)
    val sortedCircuits = finalCircuits.toSeq.sortBy(-_.nodes.length)
    sortedCircuits.take(3).map(_.nodes.length).product

  def part1(nodes: List[Node]): Int =
    val cache = cacheAllDistances(nodes)
    val edgesByDistance = cache.toSeq.sortBy(_._2).map(_._1)
    updateCircuitsUpTo(1000, edgesByDistance, nodes)

  def getFinalEdgeAndCalculateXdistance(edges: Seq[Edge], nodes: List[Node]): Int =
    @tailrec
    def go(edges: Seq[Edge], circuits: Set[Circuit]): Option[Edge] = 
      edges match
        // If no more edges to examine
        case Seq() => None
        case head :: tail =>
          val updatedCircuits = 
            if circuits.exists {circuit => circuit.contains(head)} then circuits
            else updateCircuits(head, circuits)
          if (updatedCircuits.size == 1) && (circuits.size > 1) then Some(head)
          else go(tail, updatedCircuits)

    val circuits = nodes.map(n => Circuit(List(n))).toSet
    val finalEdge = go(edges, circuits).head
    finalEdge.n1.x * finalEdge.n2.x


  def part2(nodes: List[Node]): Int = 
    val cache = cacheAllDistances(nodes)
    val edgesByDistance = cache.toSeq.sortBy(_._2).map(_._1)
    getFinalEdgeAndCalculateXdistance(edgesByDistance, nodes)

@main def main(): Unit =
  val input = readInput(8)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
