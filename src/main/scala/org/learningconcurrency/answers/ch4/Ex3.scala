package org.learningconcurrency
package answers
package ch4

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// Ex-4.3: FutureWithExists implemented in terms of Future composition
// [Upon seeing the ref. answer after coding is done]
// Changes:
// 1. The code is written after seeing the answer. Essentially, the idea of
//    implicit class is the key.
// Notes:
// 1. Our test code is better.
object Ex3 extends App {
  implicit class FutureWithExists[T](val future: Future[T]) {
    def exists(p: T => Boolean) = future.map(p)
  }

  // TODO: duplicated test code below for ex3-5
  // We shall find a way to parameterize the implicit class above and share
  // the common test code.
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
    /* Alternatively, we could fetch and handle final results this way:
    try {
      val b = Await.result(f1.exists(_ > 0), Duration.Inf)
      log(s"FutureWithExists.exists(): Future {10/$i} is positive? => $b ")
    } catch {
      case ex: Exception =>
        log(s"FutureWithExists.exists(): Future {10/$i} failed: $ex")
    }
    */
  }
  for (f <- futures) {
    Await.ready(f, Duration.Inf)
  }
}
