package org.learningconcurrency
package answers
package ch4

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// Ex-4.4: FutureWithExists implemented in terms of Promise
// [Upon seeing the ref. answer after coding is done]
// Changes:
// 1. Previously, to assign Future results to Promise, we use trySuccess() and
//    tryFailure(). Yet they shall be used when completing assignments are
//    possible or even expected; caller uses the return values to figure out
//    whether the assignment succeeded or not. But here we don't expect
//    completion and don't care about the outcome. Hence success() and
//    failure() is used instead.
object Ex4 extends App {
  implicit class FutureWithExists[T](val future: Future[T]) {
    def exists(predicate: T => Boolean): Future[Boolean] = {
      val p = Promise[Boolean]
      future foreach {
        case v => p.success(predicate(v))
      }
      future.failed foreach {
        case ex => p.failure(ex)
      }
      p.future
    }
  }

  var futures = List.empty[Future[Boolean]]
  for (i <- -5 to 5) {
    val f1 = Future { 10 / i }
    val f2 = f1.exists(_ > 0)
    f2 foreach {
      case b =>
        log(s"FutureWithExists.exists(): Future {10/$i} is positive? => $b ")
    }
    f2.failed foreach {
      case ex: Throwable =>
        log(s"FutureWithExists.exists(): Future {10/$i} failed: $ex")
    }
    futures ::= f2
  }
  for (f <- futures) {
    Await.ready(f, Duration.Inf)
  }
}
