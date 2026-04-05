def readInput(day: Int): List[String] =
  val padded = f"day$day%02d"
  scala.io.Source.fromFile(s"inputs/$padded.txt").getLines().toList
