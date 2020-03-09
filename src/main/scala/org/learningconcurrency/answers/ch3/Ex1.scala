package org.learningconcurrency
package answers
package ch3

import scala.concurrent._

// ex3-1: PiggybackContext
// This impl. differs from the ref. in that:
// 1) The ref. uses the idiom Try (...) match { case Success/Failure ... }.
//    here we do nothing on success and hence simply uses try-catch clauses.
// 2) The context instance is initialized on need.
object Ex1 extends App {

  class PiggybackContext extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = {
      try {
        runnable.run()
      } catch {
        case e: Exception => reportFailure(e)
      }
    }

    override def reportFailure(cause: Throwable): Unit = {
      println(s"Execution failed: $cause")
    }
  }

  private var context: Option[PiggybackContext] = None

  def run(body: =>Unit): Unit = {
    if (context.isEmpty) { // lazy initialization
      println("#INFO: PiggybackContext() gets initialized")
      context = Some(new PiggybackContext())
    }
    context.get.execute(new Runnable {
      override def run() = body
    })
  }

  run(3/0)
  run(throw new Exception("test exception"))
  run(println("just fine"))
}
