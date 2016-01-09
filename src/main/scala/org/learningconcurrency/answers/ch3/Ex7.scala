package org.learningconcurrency
package answers
package ch3

import scala.collection.mutable.ArrayBuffer

// ex3.7: SyncConcurrentMap, concurrent map implemented in terms of synchronized.
// Notes:
// 1. Iterators: after hasNext() returning true, a value is reserved and returned
//    upon next() is called afterwords. This is to ensure consistency in
//    concurrency. But it also means that if you clear the map after hasNext() is
//    called, calling hasNext() and next() afterwords will still indicate
//    existence of a value.
// [Upon seeing the ref. answer after coding is done]
// Changes:
// 1. In replace(A, B, B) and remove(A, B), while testing whether the old value
//    equals the expected value, we naively used plain 'if (v == oldvalue)'.
//    They are changed to test nullity, use equals() method.
// Notes:
// 1. The ref. code does not take care of iterator consistency like we do.
//    Also, test code does not cover that part. In this sense, our code is
//    somehow superior.
object Ex7 extends App {
  class SyncConcurrentMap[A, B] extends scala.collection.concurrent.Map[A, B] {

    private val underlying = scala.collection.mutable.Map[A, B]()

    def -=(key: A) = underlying.synchronized {{underlying -= key }; this}

    def +=(kv: (A, B)) = underlying.synchronized {underlying += kv; this}

    def get(key: A) = underlying.synchronized {underlying.get(key)}

    def replace(k: A, oldvalue: B, newvalue: B): Boolean = underlying.synchronized {
      underlying.get(k) match {
        case Some(v) if ((v != null) && v.equals(oldvalue)) || ((v == null) && (oldvalue == null)) =>
          underlying(k) = newvalue
          true
        case _ => false
      }
    }

    def replace(k: A, v: B): Option[B] = underlying.synchronized {
      underlying.get(k) match {
        case Some(x) =>
          underlying(k) = v
          Some(x)
        case None => None
      }
    }

    override def remove(k: A): Option[B] = underlying.synchronized {underlying.remove(k)}

    override def remove(k: A, v: B): Boolean = underlying.synchronized {
      underlying.get(k) match {
        case Some(x) if ((x != null) && x.equals(v)) || ((x == null) && (v == null)) =>
          underlying.remove(k)
          true
        case _ => false
      }
    }

    def putIfAbsent(k: A, v: B): Option[B] = underlying.synchronized {
      val x = underlying.get(k)
      underlying.put(k, v)
      x
    }

    def iterator: Iterator[(A, B)] = new Iterator[(A, B)] {
      val iter = underlying.iterator
      // After a call to hasNext() returning true, the value will be cached
      // and returned when next() is called afterwords.
      var cached: Option[(A, B)] = None

      override def hasNext = {
        if (cached.isDefined) { // called twice without next() in between?
          true
        } else underlying.synchronized {
          if (iter.hasNext) {
            cached = Some(iter.next())
            true
          } else {
            false
          }
        }
      }

      override def next() = {
        if (cached.isDefined) {
          val v = cached.get
          cached = None
          v
        } else underlying.synchronized { // called twice without hasNext() in between?
          underlying.synchronized {
            iter.next()
          }
        }
      }
    }
  }

  val map = new SyncConcurrentMap[String, Int]()

  import org.learningconcurrency.ch2.thread

  def dump(iter: Iterator[(String, Int)]): String = {
    val buf = new ArrayBuffer[String]()
    while (iter.hasNext) {
      val kv = iter.next()
      buf += s"{${kv._1}=>${kv._2}}"
    }
    buf.mkString(", ")
  }

  thread {
    for (i <- 0 to 100) {
      Thread.sleep((Math.random * 10).toInt)
      val k = s"item $i"
      map += ((k, i))
      log(s"Inserted: {$k=>$i}")
    }
  }
  thread {
    for (i <- 0 to 100) {
      Thread.sleep((Math.random * 1000).toInt)
      for (j <- 0 to 100) {
        val k = s"item $j"
        val v = map.remove(k)
        if (v.isDefined) {
          log(s"Removed: {$k=>$v}")
        }
      }
    }
  }
  thread {
    for (i <- 0 to 1000) {
      Thread.sleep((Math.random * 100).toInt)
      log(s"Dumped: ${dump(map.iterator)}")
    }
  }
}
