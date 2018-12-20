package day4

object Day4 {
  def main(argv: Array[String]) = {
    val inputFile = "input.txt" 
    val sortedLogs = (new Day4InputParser(inputFile)).getSortedInput
    val guardMetricAggregator = new GuardMetricAggregator(sortedLogs)
    val guardMostAsleep = guardMetricAggregator.getGuardWithMostTimeAsleep
    val minuteMostAsleep = guardMetricAggregator.getMinuteGuardIsMostAsleep(guardMostAsleep)
    val checksum = guardMostAsleep * minuteMostAsleep
    println(s"Gaurd that sleeps the most: $guardMostAsleep")
    println(s"Minute he is most asleep: $minuteMostAsleep")
    println(s"Checksum: $checksum")
  }
}

