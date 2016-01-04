package org.learningconcurrency
package answers
package ch3

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent._

// ex3-5: LazyCell
// [Upon seeing the ref. answer after coding is done]
// Changes:
// 1. LazyCellV2 is added. the functionality of _bitmap can be taken by _obj
//    by declaring the latter as @volatile.
object Ex5 extends App {
  class LazyCell[T](initialization: =>T) {
    @volatile private var _bitmap = false
    private var _obj: Option[T] = null
    def apply(): T = if (_bitmap) _obj.get else this.synchronized {
      if (!_bitmap) {
        _obj = Some(initialization)
        _bitmap = true
      }
      _obj.get
    }
  }

  class LazyCellV2[T](initialization: =>T) {
    @volatile private var _obj: Option[T] = None
    def apply(): T = _obj match {
      case Some(v) => v
      case None => this.synchronized {
        if (_obj.isEmpty) {
          _obj = Some(initialization)
        }
        _obj.get
      }
    }
  }

  val x = new LazyCell[Int]({
    val v = 3
    println("#Info: working for a lazy Int ...")
    Thread.sleep(1000)
    v
  })
  var y = new LazyCellV2[String]({
    val v = "OK"
    Thread.sleep(3000)
    println("#Info: working for a lazy String ...")
    v
  })
  println(s"fetch lazy string: ${y.apply()}")
  println(s"fetching lazy Int: ${x.apply()}")
}
