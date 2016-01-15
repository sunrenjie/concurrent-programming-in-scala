package org.learningconcurrency
package answers
package ch4

import scala.concurrent._
import scala.util.control.Breaks._
import scala.concurrent.duration._

// Ex-4.2: IVar: a single-assignment variable class
//
// [Upon seeing the ref. answer after coding is done]
// Changes:
// 1. We know that our Promise does not block at all, so for the duration in
//    call to Await.result(), we arbitrarily used 1.seconds. Turned out that
//    Duration.Inf shall be used.
object Ex2 extends App {
  class IVar[T] {
    private val store = Promise[T]

    def apply(): T = {
      if (!store.isCompleted)
        throw new RuntimeException(s"cannot return a value since the variable is not assigned")
      Await.result(store.future, Duration.Inf)
    }

    def :=(x: T): Unit = {
      if (!store.trySuccess(x))
        throw new RuntimeException(s"cannot assign value $x as the variable is already assigned")
    }
  }

  import org.learningconcurrency.ch2.thread

  var ivar = new IVar[Int]
  for (i <- 0 to 10) {
    thread {
      val t = (Math.random * 1000).toInt
      Thread.sleep(t)
      try {
        ivar := i
        log(s"#Info: assigning int $i to IVar (after sleeping $t ms) succeeded")
      } catch {
        case ex: Exception =>
          log(s"#Info: assigning int $i to IVar (after sleeping $t ms) failed: $ex")
      }
    }
  }
  breakable {
    while(true) {
      try {
        log(s"#Info: final value: ${ivar.apply()}")
        break()
      } catch {
        case ex: Exception => log(s"Info: final value not yet available: $ex")
      }
      Thread.sleep(10)
    }
  }
}
