package counter

import org.scalatest._

class EventCounterTest extends FlatSpec with Matchers {
    
  "The EventCounter object" should "return 0 if no events have been counted" in {
    val counter = EventCounter()
    counter.getCount() shouldEqual 0
  }
  
  it should "count an event" in {
    val counter = EventCounter()
    counter.countEvent()
    counter.getCount() shouldEqual 1
  }
  
  it should "decrease the count once an event is out of the timeframe" in {
    val counter = EventCounter(1)
    counter.countEvent()
    counter.getCount() shouldEqual 1
    Thread.sleep(2000) // wait 2 seconds
    counter.getCount() shouldEqual 0
  }
  
  it should "remove old buckets after they are no longer in the upper bound" in {
    val counter = EventCounter(1)
    counter.countEvent()
    counter.eventBuckets.size shouldEqual 1
    Thread.sleep(2000) // wait 2 seconds so old buckets will be out of timeframe
    counter.countEvent()
    counter.eventBuckets.size shouldEqual 1
  }
  
  it should "handle 10 million events in 10 seconds" in {
    val counter = EventCounter()
    var count = 0
    while (count < 10000000) {
      counter.countEvent()
      count += 1
    }
    counter.getCount(10) shouldEqual 10000000
  }
  
  it should "throw an error if requesting a negative timespan" in {
    val counter = EventCounter()
    counter.countEvent()
    an [IllegalArgumentException] should be thrownBy counter.getCount(-1)
  }  
  
  it should "throw an error if requesting more than the upper limit" in {
    val counter = EventCounter()
    an [IllegalArgumentException] should be thrownBy counter.getCount(301)
  }
  
  it should "throw an error if passing in an upper bound greater than 5 minutes (300 seconds)" in {
    a [RuntimeException] should be thrownBy EventCounter(301)
  }
  
  it should "throw an error if passing in a negative upper bound" in {
    a [RuntimeException] should be thrownBy EventCounter(-100)
  }  
}
