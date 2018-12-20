package day4

import scala.io.Source

sealed trait GuardEvent

case class ShiftBeginsEvent(guardNumber: Int) extends GuardEvent
case class GuardFallsAsleep() extends GuardEvent
case class GuardWakesUp() extends GuardEvent

class Timestamp(
  val year: Int,
  val month: Int,
  val day: Int,
  val hour: Int,
  val minute: Int
) {
  def compareTo(timestamp: Timestamp): Boolean = {
    if (year != timestamp.year) {
      year < timestamp.year
    } else if (month != timestamp.month) {
      month < timestamp.month
    } else if (day != timestamp.day) {
      day < timestamp.day
    } else if (hour != timestamp.hour) {
      hour < timestamp.hour
    } else if (minute != timestamp.minute) {
      minute < timestamp.minute
    } else {
      throw new Exception("Found Two Logs With The Same Time!")
    }
  }

  override def toString(): String = {
    s"$year-$month-$day $hour:$minute"
  }
}

class LogEntry(val timestamp: Timestamp, val event: GuardEvent) {
  def compareTo(logEntry: LogEntry): Boolean = {
    return timestamp.compareTo(logEntry.timestamp)
  }

  override def toString(): String = {
    s"$timestamp $event"
  }
}

class Day4InputParser(inputFileName: String) {
  def getSortedInput() = {
    val unsortedInput = getParsedInput
    val sortedInput = sortInputLogs(unsortedInput)
    sortedInput
  }

  private def sortInputLogs(logs: Iterator[LogEntry]) = {
    logs.toSeq.sortWith(_.compareTo(_))
  }

  private def getParsedInput(): Iterator[LogEntry] = {
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
    new LogEntry(timestamp, event)
  }

  /** Parses a timestamp String
   *  ex: "1518-03-08 00:33"
   */
  private def parseTimeString(dateTimeString: String): Timestamp = {
    val Array(dateString: String, timeString: String) = dateTimeString.split(" ")
    val Array(yearString: String, monthString: String, dayString: String) = dateString.split("-")
    val Array(hourString: String, minuteString: String) = timeString.split(":")
    new Timestamp(
      yearString.toInt,
      monthString.toInt,
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
