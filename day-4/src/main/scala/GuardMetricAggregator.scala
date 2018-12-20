package day4

case class GuardNap(start: Timestamp, end: Timestamp, duration: Int)
case class GuardTimestampAccumulator(currentGuard: Int, timestamps: Map[Int, List[Timestamp]])
case class TimestampToNapAccumulator(fallAsleepTime: Option[Timestamp], naps: List[GuardNap])

class GuardMetricAggregator(val sortedLogs: Seq[LogEntry]) {
  def getMinuteGuardIsMostAsleep(guardId: Int): Int = {
    val guardNaps = getGuardIdsToNaps.get(guardId)
    guardNaps match {
      case Some(naps) => getMinuteMostAsleepFromNaps(naps)._2
      case None => throw new Exception("This Guard Didn't Nap!")
    }
  }

  def getMostAsleepMinuteByAnyGuard(): (Int, (Int, Int)) = {
    getGuardIdsToNaps
      .map({
        case (id, naps) => (id, getMinuteMostAsleepFromNaps(naps))
      })
      .reduceLeft((acc, value) => {
        if (acc._2._2 > value._2._2) acc else value
      })
  }

  def getMinuteMostAsleepFromNaps(naps: List[GuardNap]): (Int, Int) = {
    val minutesAsleep = naps.flatMap(napToMinutesAsleep(_))
    val initialValue: Map[Int, Int] = Map()
    val minuteToCount = minutesAsleep.foldLeft(initialValue)((acc, value) => {
      val previousCount = acc.getOrElse(value, 0)
      acc + (value -> (previousCount + 1))
    })
    minuteToCount.foldLeft((0, 0))((acc, entry) => {
      entry match {
        case (k, v) => if (v > acc._2) entry else acc
      }
    })
  }

  def napToMinutesAsleep(nap: GuardNap): List[Int] = {
    val GuardNap(start, end, duration) = nap
    0.to(duration - 1).map(i => {
      val startPlusIncr = start.minute + i
      startPlusIncr % 60
    }).toList
  }

  def getGuardWithMostTimeAsleep(): Int = {
    val guard = getGuardsToTotalTimeAsleep
      .foldLeft(None: Option[(Int, Int)])((acc, entry) => {
        acc match {
          case Some(guardWithMostSoFar) =>
            if (entry._2 > guardWithMostSoFar._2) Some(entry) else acc
          case None => Some(entry)
        }
      })
    guard match {
      case Some((id, _)) => id
      case _ => throw new Exception("No Guard Found!")
    }
  }

  def getGuardsToTotalTimeAsleep(): Map[Int, Int] = {
    getGuardIdsToNaps.map({
      case (k, v) => (k, convertNapsToTotalTimeAsleep(v)) 
    })
  }

  def convertNapsToTotalTimeAsleep(naps: List[GuardNap]): Int = {
    naps.foldLeft(0)(_ + _.duration)
  }

  def getGuardIdsToNaps(): Map[Int, List[GuardNap]] = {
    getGuardIdToOrderedNapEvents.map({
      case (k, v) => (k, convertTimestampsToNaps(v)) 
    })
  }

  def convertTimestampsToNaps(timestamps: List[Timestamp]): List[GuardNap] = {
    val initalValue = TimestampToNapAccumulator(None: Option[Timestamp], List())
    timestamps.foldLeft(initalValue)((acc, timestamp) => {
      acc match {
        case TimestampToNapAccumulator(None, naps) =>
          TimestampToNapAccumulator(Some(timestamp), naps)
        case TimestampToNapAccumulator(Some(timestamp2), naps) => {
          val guardNap = GuardNap(timestamp, timestamp2, timestamp2 getMinuteDiff timestamp)
          TimestampToNapAccumulator(None, guardNap :: naps)
        }
      }
    }).naps
  }

  def getGuardIdToOrderedNapEvents(): Map[Int, List[Timestamp]] = {
    val firstEvent = sortedLogs.head.event
    val firstGuard = firstEvent match {
      case ShiftBeginsEvent(id) => id
      case _ =>
        throw new Exception("The First Event Is Not A Guard Beginning A Shift!")
    }
    val initialState = GuardTimestampAccumulator(firstGuard, Map[Int, List[Timestamp]]())
    sortedLogs.foldLeft(initialState)(logEntryReducer(_, _)).timestamps
  }

  def logEntryReducer(
    acc: GuardTimestampAccumulator,
    logEntry: LogEntry
  ): GuardTimestampAccumulator = {
    // The current Guard changes when a new guard begins a shift
    val currentGuardId = logEntry.event match {
      case ShiftBeginsEvent(guardNumber) => guardNumber
      case _ => acc.currentGuard
    }
    val guardCurrentTimestamps = acc.timestamps.getOrElse(currentGuardId, List[Timestamp]())
    val newGuardTimestamps = logEntry.event match {
      case GuardFallsAsleep() => logEntry.timestamp :: guardCurrentTimestamps
      case GuardWakesUp() => logEntry.timestamp :: guardCurrentTimestamps
      case _ => guardCurrentTimestamps
    }
    val newTimestamps = acc.timestamps + (currentGuardId -> newGuardTimestamps)
    GuardTimestampAccumulator(currentGuardId, newTimestamps)
  }
}
