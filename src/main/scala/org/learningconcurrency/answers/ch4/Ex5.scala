package org.learningconcurrency
package answers
package ch4

import scala.async.Async.{async, await}
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// Ex-4.5: FutureWithExists implemented in terms of Async framework
// [Upon seeing the ref. answer after coding is done]
// Changes:
// 1. For wait for the Future result, we previously used
//    Await.result(future, Duration.Inf). While this is certainly correct,
//    using await() together with async() is better.
object Ex5 extends App {
  implicit class FutureWithExists[T](val future: Future[T]) {
    def exists(predicate: T => Boolean): Future[Boolean] = async {
      val v = await { future }
      predicate(v)
    }
  }

  var futures = List.empty[Future[Boolean]]
  for (i <- -5 to 5) {
    val f1 = Future { 10/ i }
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
