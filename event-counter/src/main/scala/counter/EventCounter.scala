package counter

import scala.collection.mutable.Map

/** Factory for EventCounter instances */
object EventCounter {
  
  def apply(upperBound: Int = 300) = new EventCounter(upperBound)
  
}

/** An event counter to keep track of the number of times an event occurred in a recent timespan.
 *  
 *  Contains a map of event buckets, where each bucket represents one timestamp (in seconds). 
 *  Each time an event occurs, the count for the current second's bucket is incremented by one. 
 *  Once a bucket falls outside the maximum timespan (upper bound), it is removed from the map.
 *  
 *  @constructor Create a new event counter
 *  @param upperBound  The max timespan we can go back (in seconds), defaults to 300
 */
class EventCounter(upperBound: Int) {
  require(upperBound <= 300 && upperBound > 0)
  
  val eventBuckets: Map[Long,Int] = Map()
  
  def nowInSeconds() = (System.currentTimeMillis / 1000).toInt
  
  var oldestTimestamp = nowInSeconds()
  
  /** Increments the count by one */
  def countEvent() = {
    val now = nowInSeconds()
    val updatedCount = eventBuckets.getOrElse(now, 0) + 1
    eventBuckets(now) = updatedCount
    clearExpiredBuckets()
  }
  
  /** Sums up the counts from each timestamp bucket in the specified timespan
   *  
   *  @param timeframe	The number of seconds to go back for the count
   *  @return Int
   */
  def getCount(timespan: Int = upperBound) = {
    if (timespan > upperBound || timespan <= 0) {
      throw new IllegalArgumentException(s"Invalid timespan: must be between 1 and $upperBound seconds")
    }
    val now = nowInSeconds()
    val bound = now - timespan
    val range = bound to now
    range.foldLeft(0) { (previousSum, timestamp) => {
        previousSum + eventBuckets.getOrElse(timestamp, 0)
      }
    }
  }  
  
  /** Removes all map entries for timestamps that are outside of the upper bound for the timespan.
   *  This happens at most once per second, and never has to go through more than (upper bound + 1) entries.
   */
  private def clearExpiredBuckets() = {
    val now = nowInSeconds()
    val expireTime = now - upperBound
    if (oldestTimestamp < expireTime) {
      (oldestTimestamp until expireTime).foreach(timestamp => eventBuckets remove timestamp)
      oldestTimestamp = expireTime
    }
  }
}