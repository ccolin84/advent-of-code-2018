import scala.io.Source

object Day4 {
  def main(argv: Array[String]) = {
    val inputFile = "input.txt" 
    val inputParser = new Day4InputParser(inputFile)
    val unsortedLogs = inputParser.getParsedInput
    unsortedLogs.take(5).foreach(println(_))
  }
}

sealed trait GuardEvent

case class ShiftBeginsEvent(guardNumber: Int) extends GuardEvent
case class GuardFallsAsleep() extends GuardEvent
case class GuardWakesUp() extends GuardEvent

case class Timestamp(
  year: Int,
  day: Int,
  hour: Int,
  minute: Int
)

case class LogEntry(
  timeStamp: Timestamp,
  event: GuardEvent
)

class Day4InputParser(inputFileName: String) {
  def getSortedInput() = {
  }

  def getParsedInput(): Iterator[LogEntry] = {
    Source
      .fromFile(inputFileName)
      .getLines
      .map(parseInputLine(_))
  }

  private def parseInputLine(line: String): LogEntry = {
    val Array(timeSection, guardEventString) = line.split("] ")
    val dateTimeString = timeSection.filter(_ != '[')
    val event = parseGuardEventString(guardEventString) 
    val timestamp = parseTimeString(dateTimeString)
    LogEntry(timestamp, event)
  }

  /** Parses a timestamp String
   *  ex: "1518-03-08 00:33"
   */
  private def parseTimeString(dateTimeString: String): Timestamp = {
    val Array(dateString: String, timeString: String) = dateTimeString.split(" ")
    val Array(yearString: String, monthString: String, dayString: String) = dateString.split("-")
    val Array(hourString: String, minuteString: String) = timeString.split(":")
    Timestamp(
      yearString.toInt,
      dayString.toInt,
      hourString.toInt,
      minuteString.toInt
    )
  }

  /** Parses a String describing a guard event 
   *  ex: "Guard #123 begins shift"
   *  ex: "falls asleep"
   *  ex: "wakes up"
   */
  private def parseGuardEventString(eventString: String): GuardEvent = {
    val event: Option[GuardEvent] = if (eventString contains "begins shift") {
      val guardNumber = eventString
        .split(" ")(1)
        .filter(_ != '#')
        .toInt
      Some(ShiftBeginsEvent(guardNumber))
    } else if (eventString contains "falls asleep") {
      Some(GuardFallsAsleep())
    } else if (eventString contains "wakes up") {
      Some(GuardWakesUp())
    } else {
      None
    }
    event match {
      case Some(event) => event
      case None => throw new Exception(s"Unrecognized Event: $eventString")
    }
  }
}
