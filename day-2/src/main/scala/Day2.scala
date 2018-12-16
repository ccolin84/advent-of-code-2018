import scala.io.Source

object Day2 {
  def main(args: Array[String]) = {
    val inputFile = "src/input.txt"
    val checksum = getChecksum(inputFile)
    println(s"The checksum is $checksum")
    val matchingLetters = getMatchingLettersForCorrectBoxes(inputFile)
    println(s"The matching letters are: $matchingLetters")
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

  def getMetricsFromBoxId(line: String): Tuple3[Boolean, Boolean, String] = {
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
    (twoOfAChar.nonEmpty, threeOfAChar.nonEmpty, line)
  }

  def getValidBoxIds(inputFile: String): Tuple2[Set[String], Set[String]] = {
    var twoMatchingCharsBoxIds = Set[String]()
    var threeMatchingCharsBoxIds = Set[String]()
    foreachInputFileLine(inputFile, line => {
      var (twoCount, threeCount, boxId) = getMetricsFromBoxId(line)
      if (twoCount) twoMatchingCharsBoxIds += boxId
      if (threeCount) threeMatchingCharsBoxIds += boxId
      true
    })
    return (twoMatchingCharsBoxIds, threeMatchingCharsBoxIds)
  }

  def getChecksum(inputFile: String): Int = {
    var (twoMatchingChars, threeMatchingChars) = getValidBoxIds(inputFile)
    return twoMatchingChars.size * threeMatchingChars.size
  }

  def getStrWithoutPositionalMismatchingChars(s1: String, s2: String) = {
    s1
      .zip(s2)
      .foldRight("")((charPair, acc) =>
        charPair match {
          case (c1, c2) => if (c1 == c2) c1 + acc else acc
        }
      )
  }

  def getMatchingLettersForCorrectBoxes(inputFile: String): String = {
    var (twoMatchingChars, threeMatchingChars) = getValidBoxIds(inputFile)
    var validBoxes = twoMatchingChars | threeMatchingChars
    var matchingChars: Option[String] = None
    var validBoxIds = validBoxes.toList

    validBoxIds
      .zipWithIndex
      .withFilter(? => matchingChars == None)
      .foreach({
        case (boxId, inx) => {
          val remainingBoxes = validBoxIds.drop(inx + 1)
          remainingBoxes
            .withFilter(? => matchingChars == None)
            .foreach(innerBoxId => {
              val strWithoutMismatchingChars = getStrWithoutPositionalMismatchingChars(boxId, innerBoxId)
              if (strWithoutMismatchingChars.size == (boxId.size - 1)) {
                matchingChars = Some(strWithoutMismatchingChars)
              }
            })
        }
      })

    matchingChars match {
      case Some(answer) => answer
      case None => "No matching boxes found!"
    }
  }
}
