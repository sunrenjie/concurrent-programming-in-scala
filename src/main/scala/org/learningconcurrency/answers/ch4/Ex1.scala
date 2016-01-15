package org.learningconcurrency
package answers
package ch4

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.io.StdIn
import scala.io.Source
import scala.util.control.Breaks._
import scala.concurrent.duration._
import java.util.{TimerTask, Timer}
import org.learningconcurrency.ch4.PromisesCancellation.cancellable

// Ex-4.1:
// An app that accepts an URL from stdin, fetch the HTML code and write to
// stdout. The action times out after a certain amount of time. During the
// wait, it prints a '.' character to stdout periodically to entertain the
// user.
// [Upon seeing the ref. answer after coding is done]
// Notes:
// 1. The ref. solution encodes timeout directly in future. This is no doubt
//    much easier. But we've decided not modify our code for that matter.
object Ex1 extends App {
  private val timer = new Timer(true)

  // Code is duplicated from org.learningconcurrency.ch4.PromisesAndTimers.
  // We cannot directly use timeout(): the timer will be uninitialized.
  def timeout(millis: Long): Future[Unit] = {
    val p = Promise[Unit]
    timer.schedule(new TimerTask {
      def run() = p.success(())
    }, millis)
    p.future
  }

  def fetcher(url: String, timeWaited: Long): String = {
    val cancelled = false
    val (cancel, value) = cancellable({ cancel =>
      while (true) {
        // Sleep and then check; the caller may just cancel without the need
        // to wait for completion.
        Thread.sleep(50)
        if (cancel.isCompleted) throw new CancellationException
        System.out.print('.')
      }
    })

    val f = Future {
      val txt = Source.fromURL(url).mkString
      val maxLen = 50
      if (txt.length <= maxLen) {
        txt
      } else {
        s"#Result: " + txt.substring(0, maxLen - 3) + "..."
      }
    }
    try {
      val ret = Await.result(f, timeWaited.milliseconds)
      cancel.trySuccess(())
      ret
    } catch {
      case ex: Exception =>
        cancel.trySuccess(())
        s"#Error: $ex"
    }
  }

  breakable {
    while (true) {
      var ln = StdIn.readLine("Please input a URL and press ENTER: ")
      if (ln == null) {
        println("")
        log("#Info: empty input; will exit.")
        break()
      } else {
        ln = ln.trim()
        if (ln.equals("")) {
          println("")
          log("#Error: invalid URL")
        } else {
          val txt = fetcher(ln, 2000)
          println("")
          log(txt)
        }
      }
    }
  }
}
