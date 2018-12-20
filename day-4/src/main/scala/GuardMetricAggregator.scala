package day4

case class GuardNap(start: Timestamp, end: Timestamp, length: Int)

class GuardMetricAggregator(val sortedLogs: Seq[LogEntry]) {
  def getGuardIdToNaps() = {
  }  

  def getGuardsToMinutesAsleep(sortedLogs: Seq[LogEntry]): Map[Int, List[Timestamp]] = {
    val initialMap = Tuple2[Option[Int], Map[Int, List[Timestamp]]](
      None,
      Map[Int, List[Timestamp]]()
    )
    sortedLogs.foldLeft(initialMap)(logEntryReducer(_, _))._2
  }

  def logEntryReducer(
    acc: Tuple2[Option[Int], Map[Int, List[Timestamp]]],
    logEntry: LogEntry
): Tuple2[Option[Int], Map[Int, List[Timestamp]]] = {
    val currentGuardId = (logEntry.event, acc._1) match {
      case (ShiftBeginsEvent(guardNumber), _) => guardNumber
      case (_, Some(id)) => id
      case _ =>
        throw new Exception(s"Unable To Determine A Guard Id For Log Entry: $logEntry, $acc")
    }
    val guardCurrentTimestamps = acc._2.getOrElse(currentGuardId, List[Timestamp]())
    val newGuardTimestamps = logEntry.event match {
      case GuardFallsAsleep() => logEntry.timestamp:: guardCurrentTimestamps
      case _ => guardCurrentTimestamps
    }
    val newMap = acc._2 + (currentGuardId -> newGuardTimestamps)
    (Some(currentGuardId), newMap)
  }
}
