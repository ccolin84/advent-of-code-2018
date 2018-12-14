import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val totalChangeFromFile = calculateTotalFromFile(inputFile)
    println(s"The total change from the input file is: $totalChangeFromFile")
  }

  val inputFile = "src/input.txt"

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
    }
  }

  def calculateTotalFromFile(fileName: String) = {
    val bufferedSource = Source.fromFile(fileName)
    try {
      var total = 0
      for (line <- bufferedSource.getLines) {
        total = updateTotalFromLine(total, line)
      }
      total
    } catch {
      case e: Exception => {
        println("Something went wrong!")
      }
    } finally {
      bufferedSource.close
    }
  }
}
