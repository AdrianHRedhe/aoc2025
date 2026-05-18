package day11

import util.readInput
import scala.collection.immutable.Queue
import scala.annotation.tailrec

object Solution:
  type Graph = Map[String, List[String]]

  def parse(input: List[String]): Graph =
    input.map {s => s.split(":") match
      case Array(node, children) =>
        (node -> children.split(" ").toList) 
    }.toMap

  def getInDegrees(graph: Graph): Map[String, Long] =
    val initial = graph.keys.map(node => node -> 0L).toMap
    graph.values.flatten.foldLeft(initial) { (acc, node) =>
      acc + (node -> (acc.getOrElse(node, 0L) + 1L))
    }

  def topologicalSort(graph: Graph): List[String] =
    @tailrec
    def loop(q: Queue[String], orderedNodes: List[String], inDegrees: Map[String, Long], graph: Graph): List[String] =
      q.dequeueOption match
        case None => orderedNodes
        case Some(node, qRest) =>
          val children = graph.getOrElse(node, List())
          val newInDegrees = children.foldLeft(inDegrees) { (acc, child) =>
            acc + (child -> (acc.getOrElse(child, 0L) - 1L))
          }
          loop(
            qRest ++ newInDegrees.collect {case (node, v) if children.contains(node) && v == 0L => node}, 
            node :: orderedNodes, 
            newInDegrees,
            graph
          )

    val inDegrees = getInDegrees(graph)
    val q = Queue[String]() ++ inDegrees.collect {case (node, v) if v == 0L => node}
    loop(q, List(), inDegrees, graph).reverse

  def countTotalPaths(graph: Graph, orderedNodes: List[String], start: String, stop: String, nStartPaths: Long): Long =
    val pathsToNode = orderedNodes.foldLeft(Map[String, Long](start -> nStartPaths)) { 
      case (acc, node) => 
        val nodePaths = acc.getOrElse(node, 0L)
        val updates = graph.getOrElse(node, List()).map(child =>
            child -> (acc.getOrElse(child, 0L) + nodePaths)
        )
        acc ++ updates
    }
    pathsToNode.getOrElse(stop, 0L)
      
  def part1(graph: Graph): Long =
    val orderedNodes = topologicalSort(graph)
    countTotalPaths(graph, orderedNodes, "you", "out", 1)

  def countPathsPassingThrough(graph: Graph, orderedNodes: List[String], nodes: String*): Long =
    nodes.sliding(2).foldLeft(1L) { 
      case (pathCount, Seq(from, to)) =>
        countTotalPaths(graph, orderedNodes, from, to, pathCount)
    }

  def part2(graph: Graph): Long = 
    val orderedNodes = topologicalSort(graph)
    countPathsPassingThrough(graph, orderedNodes, "svr", "fft", "dac", "out")

@main def main(): Unit =
  val input = readInput(11)
  val data = Solution.parse(input)

  val part1 = Solution.part1(data)
  val part2 = Solution.part2(data)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
