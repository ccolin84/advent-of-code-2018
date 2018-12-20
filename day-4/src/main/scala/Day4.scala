package day4

object Day4 {
  def main(argv: Array[String]) = {
    val inputFile = "input.txt" 
    val inputParser = new Day4InputParser(inputFile)
    val sortedLogs = inputParser.getSortedInput
    val guardsToMinutesAsleep = getGuardsToMinutesAsleep(sortedLogs)
    println(guardsToMinutesAsleep)
    // println(s"Gaurd that sleeps the most: $guardWithMostMinutesAsleep")
    // println(s"Minute he is most asleep: $minuteMostAsleep")
    // println(s"Checksum: $checksum")
  }
}

