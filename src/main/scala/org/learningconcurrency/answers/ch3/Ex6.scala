package org.learningconcurrency
package answers
package ch3

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

// ex-3.6: PureLazyCell, a lazy value optimized for cases when the computation
// doesn't have intolerable side-effects and can be freely repeated.
// This demonstrates the power of immutability: no writes, no locks.
// [Upon seeing the ref. answer after coding is done]
// Changes:
// 1. A PureLazyCellV2 is added. We realized that @volatile is nevertheless
//    one type of lock (albeit a lightweight one); the class shall be
//    implemented in terms of AtomicReference instead.
object Ex6 extends App {
  class PureLazyCell[T](initialization: =>T) {
    @volatile private var _obj: Option[T] = None
    def apply(): T = if (_obj.isDefined) _obj.get else {
      _obj = Some(initialization)
      _obj.get
    }
  }
  class PureLazyCellV2[T](initialization: =>T) {
    private val _obj = new AtomicReference[Option[T]](None)

    @tailrec
    final def apply(): T = {
      val prev = _obj.get
      if (prev.isDefined) {
        prev.get
      } else {
        _obj.compareAndSet(prev, Some(initialization))
        apply()
      }
    }
  }

  val x = new PureLazyCellV2[Int]({
    val v = 3
    log("working for a lazy Int started")
    Thread.sleep(1000)
    log("working for a lazy Int finished")
    v
  })

  import org.learningconcurrency.ch2.thread
  for (i <- 0 to 100) {
    Thread.sleep(300)
    thread { log(s"fetched lazy Int ${x.apply()}") }
  }
}
