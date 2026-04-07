def readInput(day: Int, test: Boolean = false): List[String] =
  val padded = f"day$day%02d"
  val suffix = if test then "_test" else ""
  scala.io.Source.fromFile(s"inputs/$padded$suffix.txt").getLines().toList
