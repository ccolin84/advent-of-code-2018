import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val inputFile = "src/input.txt"
    // The answer to part 1
    val totalChangeFromFile = calculateTotalFromFile(inputFile)
    // The answer to part 2
    val firstRepeatedTotal = calculateFirstRepeatedTotalFromFile(inputFile)
    println(s"The total change from the input file is: $totalChangeFromFile")
    println(s"The first repeated total is: $firstRepeatedTotal")
  }

  def parseInputLine(line: String): Tuple2[Char, Int] = {
    val operator = line.charAt(0)
    val number = (line.drop(1)).toInt
    return (operator, number)
  }

  def updateTotalFromLine(total: Int, line: String): Int = {
    val (operator, number) = parseInputLine(line)
    operator match {
      case '+' => total + number
      case '-' => total - number
      case other => throw new Exception(s"expected + or - but found $other")
    }
  }

  def forEachTotalFromFile(fileName: String, handler: Int => Boolean): Unit = {
    val bufferedSource = Source.fromFile(fileName)
    try {
      var total = 0
      var continue = true
      bufferedSource.getLines.withFilter(? => continue).foreach(line => {
        total = updateTotalFromLine(total, line)
        continue = handler(total)
      })
    } catch {
      case e: Exception => {
        println("Something went wrong!")
      }
    } finally {
      bufferedSource.close
    }
  }

  // calculates the answer for Day 1 part 1
  def calculateTotalFromFile(fileName: String): Int = {
    var total = 0
    forEachTotalFromFile(fileName, (newTotal: Int) => {
      total = newTotal
      true
    })
    total
  }

  // calculates the answer for Day 1 part 2
  def calculateFirstRepeatedTotalFromFile(fileName: String, startingTotal: Int = 0, prevTotals: Set[Int] = Set[Int]()): Int = {
    var pastTotals = prevTotals
    var firstDuplicate: Option[Int] = None
    var lastTotal = 0
    forEachTotalFromFile(fileName, (newTotal: Int) => {
      lastTotal = newTotal + startingTotal
      if (pastTotals contains lastTotal) {
        firstDuplicate = Some(lastTotal)
        false
      } else {
        pastTotals = pastTotals + lastTotal
        true
      }
    })
    firstDuplicate match {
      case Some(answer) => answer
      case None => calculateFirstRepeatedTotalFromFile(fileName, lastTotal, pastTotals)
    }
  }
}
