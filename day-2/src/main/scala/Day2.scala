import scala.io.Source

object Day2 {
  def main(args: Array[String]) = {
    val inputFile = "src/input.txt"
    val checksum = getChecksum(inputFile)
    println(s"The checksum is $checksum")
  }

  def foreachInputFileLine(inputFile: String, handler: String => Boolean) = {
    val bufferedSource = Source.fromFile(inputFile)
    try {
      var continue = true
      bufferedSource.getLines.withFilter(? => continue).foreach(line => {
        continue = handler(line)
      })
    } catch {
      case error: Exception =>
        println("Something went wrong while scrolling through the input file")
    } finally {
      bufferedSource.close
    }
  }

  def getMetricsFromLine(line: String): Tuple2[Boolean, Boolean] = {
    var charCounts = Map[Char, Int]()
    var twoOfAChar = Set[Char]()
    var threeOfAChar = Set[Char]()
    line
      .withFilter(? => !(twoOfAChar.nonEmpty && threeOfAChar.nonEmpty))
      .foreach(char => {
        var prevCount = charCounts.getOrElse(char, 0)
        var currentCount = prevCount + 1
        charCounts += (char -> currentCount)
        if (currentCount == 2) twoOfAChar += char
        if (currentCount == 3) {
          threeOfAChar += char
          twoOfAChar -= char
        }
      })
    (twoOfAChar.nonEmpty, threeOfAChar.nonEmpty)
  }

  def getChecksum(inputFile: String): Int = {
    var totalTwoCounts = 0
    var totalThreeCounts = 0
    foreachInputFileLine(inputFile, line => {
      var (twoCount, threeCount) = getMetricsFromLine(line)
      if (twoCount) totalTwoCounts += 1
      if (threeCount) totalThreeCounts += 1
      true
    })
    return totalTwoCounts * totalThreeCounts
  }
}
