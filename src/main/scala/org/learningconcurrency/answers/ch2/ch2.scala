package org.learningconcurrency
package answers
package ch2


import java.lang.UnsupportedOperationException

import scala.annotation.tailrec
import scala.collection.mutable
import org.learningconcurrency.ch2.thread

object ch2ex1 extends App {

  // 2.1
  def parallel[A, B](a: () => A, b: () => B): (A, B) = {
    // Defined as Option[] so that they could be uninitialized; stolen from:
    // http://stackoverflow.com/questions/2440134/is-this-the-proper-way-to-initialize-null-references-in-scala
    // The textbook code use another approach:
    // var aVal: A = null.asInstanceOf[A]
    var ra = None: Option[A]
    var rb = None: Option[B]
    val ta = new Thread {
      override def run(): Unit = {
        ra = Some(a())
      }
    }
    ta.start()
    val tb = new Thread {
      override def run(): Unit = {
        rb = Some(b())
      }
    }
    tb.start()
    ta.join()
    tb.join()
    (ra.get, rb.get)
  }

  val fa = () => {
    "String"
  }
  val fb = () => {
    1
  }
  parallel(fa, fb)

}


object ch2ex2 extends App {
  // 2.2
  // This implementation is inefficient: setting the thread as daemon and
  // placing the endless loop within is much better
  def periodically(duration: Long, b: => Unit): Unit = {
    // Here for each invocation, a new Thread is created. The textbook code use a single thread instead.
    while (true) {
      val t = new Thread {
        override def run(): Unit = {
          b
        }
      }
      t.start()
      t.join()
      Thread.sleep(duration)
    }
  }

  periodically(1000, {
    println("Hello")
  })
}


object ch2ex3 extends App {

  // 2.3
  // Here the choice of target of synchronized is important. The variable 't' at
  // question cannot be used (after assigning to it, calling t.notify() will end
  // up raising java.lang.IllegalMonitorStateException), a better choice seems
  // to be to put t in a container and use the container. Here we use the whole
  // class for simplicity; note that this approach can be overkill: all methods,
  // even the most trivial ones, will subject to block.
  class SyncVar[T] {
    var t: Option[T] = None

    def get(): T = this.synchronized {
      t match {
        case Some(n) =>
          t = None
          n
        case None => throw new UnsupportedOperationException("get() from empty SyncVar object")
      }
    }

    def put(x: T): Unit = this.synchronized {
      t match {
        case Some(n) => throw new UnsupportedOperationException("put() to nonempty SyncVar object")
        case None => t = Some(x)
      }
    }

    // 2.4
    def isEmpty: Boolean = this.synchronized {
      t match {
        case None => true
        case _ => false
      }
    }

    def nonEmpty: Boolean = !this.isEmpty

    // 2.5
    def getWait: T = this.synchronized {
      while (t.isEmpty) this.wait()
      val x = t.get
      t = None
      this.notify()
      x
    }

    def putWait(x: T): Unit = this.synchronized {
      while (t.isDefined) this.wait()
      t = Some(x)
      this.notify()
    }
  }

  val x = new SyncVar[Int]()
  x.put(1)
  val t = new Thread {
    override def run(): Unit = {
      Thread.sleep(5000)
      val v = x.getWait
      println("thread getWait() value " + v.toString)
    }
  }
  t.start()
  x.putWait(3)
}


object ch2ex6 extends App {

  // 2.6 SyncVar class that hold at most n values
  class SyncQueue[T](capacity: Int) {

    import scala.collection.mutable

    private val data = mutable.Queue[T]()

    def get(): T = this.synchronized {
      if (data.isEmpty)
        throw new UnsupportedOperationException("get() from empty SyncQueue[] object")
      val x = data.dequeue()
      this.notify()
      x
    }

    def put(x: T): Unit = this.synchronized {
      if (data.length >= capacity)
        throw new UnsupportedOperationException("put() to full SyncQueue[] object")
      data.enqueue(x)
      this.notify()
    }

    def isEmpty: Boolean = this.synchronized {
      data.isEmpty
    }

    def nonEmpty: Boolean = this.synchronized {
      data.nonEmpty
    }

    def getWait: T = this.synchronized {
      while (data.isEmpty) this.wait()
      val x = data.dequeue()
      this.notify()
      x
    }

    def putWait(x: T): Unit = this.synchronized {
      while (data.length >= capacity) this.wait()
      data.enqueue(x)
      this.notify()
    }
  }

  val q = new SyncQueue[Int](3)
  val th = new Thread {
    override def run(): Unit = {
      var x = q.getWait
      println("thread: get($x) is done with val=" + x)
      x = q.getWait
      println("thread: get($x) is done with val=" + x)
    }
  }
  th.start()
  Thread.sleep(5000)

  for {x <- 1 until 4} {
    println("Main: will call put() with val=" + x)
    q.put(x)
    println("Main: will sleep for 1s")
    Thread.sleep(1000)
  }
  println("thread of 4 put() is done")
}

object ch2ex7 extends App {

  // 2.7
  // This solution assigns an unique and immutable ID for each Account during creation. Then in sendAll(),
  // the accounts are sort and recursively locked. The immutable order of accounts are locked prevents dead lock.
  // A non-recursive solution shall be based on some explicit JDK lock.

  /*
  The reference answer make use of a recursive locking technique:
  def recur(li: List[Account]): Unit = li match {
    case h :: rest => h.synchronized {
      println(h)
      recur(rest)
    }
    case _ => println("over")
  }
  recur(Account("foo", 1)::(Account("bar", 2)::Nil)

   */

  // To generate unique IDs in a synchronized manner.
  object SynchronizedProtectedUid extends App {

    var uidCount = 0

    def getUniqueId = this.synchronized {
      val freshUid = uidCount + 1
      uidCount = freshUid
      freshUid
    }

    def printUniqueIds(n: Int): Unit = {
      val uids = for (i <- 0 until n) yield getUniqueId
      println(s"Generated uids: $uids")
    }

    val t = thread {
      printUniqueIds(5)
    }
    printUniqueIds(5)
    t.join()

  }

  case class Account(val name: String, var money: Int) {
    val uid: Int = SynchronizedProtectedUid.getUniqueId
  }

  def syncRecursivelyIterAndExecute(accounts: List[Account], i: Int, job: () => Unit): Unit = {
    if (i == accounts.length) {
      job()
      return
    }
    accounts(i).synchronized {
      syncRecursivelyIterAndExecute(accounts, i + 1, job)
    }
  }

  def syncRecursivelyAndExecute(accounts: Set[Account], job: () => Unit): Unit = {
    val li = accounts.toList
    li.sortBy(a => a.uid)
    syncRecursivelyIterAndExecute(li, 0, job)
  }

  def sendAll(accounts: Set[Account], target: Account): Unit = target.synchronized {
    accounts foreach { a => assert(a.uid != target.uid) }
    syncRecursivelyAndExecute(accounts, () => {
      accounts foreach {
        a => {
          target.money += a.money
          a.money = 0
        }
      }
    })
  }

  val a = Account("Jill", 1000)
  val b = Account("Jack", 2000)
  val c = Account("Ellis", 3000)
  val d = Account("Bob", 3000)
  val e = Account("Neil", 3000)
  val f = Account("Tom", 3000)
  val set = Set(a, b, c, d, e)
  sendAll(set, f)
  println(s"f = ${f.money}")
}

object ch2ex8 extends App {

  // 2.8
  object PriorityTaskPool {

    // PriorityQueue code stolen from text book code and from:
    // http://stackoverflow.com/questions/789250/scala-is-there-a-way-to-use-priorityqueue-like-i-would-in-java
    // Policy: the worker thread will be created as user thread (isDaemon=false). Upon shutdown(), it will not
    // accept new submissions and fail with UnsupportedOperationException if there are any. After all tasks are
    // finished, it will exit.
    case class Job(task: () => Unit, pri: Int) {
      val job: () => Unit = task
      val priority: Int = pri
    }

    implicit def orderedJob(j: Job): Ordered[Job] = new Ordered[Job] {
      def compare(other: Job): Int = j.priority.compare(other.priority)
    }

    val tasks = new mutable.PriorityQueue[Job]()

    object Worker extends Thread {
      var terminated = false

      def poll(): Option[Job] = tasks.synchronized {
        while (tasks.isEmpty && !terminated) tasks.wait()
        if (tasks.isEmpty)
          None
        else
          Some(tasks.dequeue())
      }

      @tailrec override def run(): Unit = {
        // being extremely lazy to make the asynchronous nature obvious
        println("Worker thread: sleeping for a while to emulate being overloaded ...")
        Thread.sleep(300)
        poll() match {
          case Some(task) => task.job(); run()
          case None => if (!terminated) run()
        }
      }

      this.setDaemon(false)
    }

    def shutdown(): Unit = tasks.synchronized {
      Worker.terminated = true
      // To notify the thread(s) sleeping waiting for new tasks to wake up for this new event.
      tasks.notifyAll()
    }

    def asynchronous(priority: Int)(task: () => Unit): Unit = tasks.synchronized {
      if (Worker.terminated)
        throw new UnsupportedOperationException("terminated pool does not accept new job")
      tasks.enqueue(Job(task, priority))
      tasks.notify()
    }

    Worker.start()
  }

  val p = PriorityTaskPool
  Range(0, 9).reverse foreach { i => p.asynchronous(i)(() => println(s"Async job: println $i")) }
  p.shutdown()
  try {
    Range(0, 1).reverse foreach { i => p.asynchronous(i)(() => println(s"Async job after shutdown(): println $i")) }
  } catch {
    case _: UnsupportedOperationException =>
      println("An UnsupportedOperationException is thrown upon submitting new task after shutdown(), as expected.")
  }
}

object ch2ex9and10 extends App {
  // A multi-thread pool that executes jobs in the order based on descending importance values.
  // Upon shutdown(), no new jobs will be accepted; existing jobs

  // TODO this solution is correct, yet over-complicated
  // Actually, we could go without the unimportant flag, along with the complex
  // logic around it: whenever a low-priority tasks is found, we could simply
  // empty the queue. Quoting Wikipedia [1]:
  // "In practice, it is often necessary to keep performance goals in mind when
  // first designing software, but the programmer balances the goals of design
  // and optimization."
  // [1] https://en.wikipedia.org/wiki/Program_optimization
  // 2.9 and 2.10
  class PriorityTaskPoolWithMultipleThreads(numThreads: Int, importance: Int) {

    case class Job(task: () => Unit, pri: Int) {
      val job: () => Unit = task
      val priority: Int = pri
    }

    implicit def orderedJob(j: Job): Ordered[Job] = new Ordered[Job] {
      def compare(other: Job): Int = j.priority.compare(other.priority)
    }

    val tasks = new mutable.PriorityQueue[Job]()
    // When terminated is set to true (by shutdown()), asynchronous() will not
    // accept new tasks. Threads in the list will only process tasks with
    // priority higher than importance; when a poll()ed task with lower priority
    // is found, unimportant will be set to true, in which case, all tasks left
    // (if there are any) will not deserve processing (since this is a priority
    // queue, they will have even lower priorities), all peer threads will exit
    // immediately without poll()ing them.
    var terminated = false
    var unimportant = false

    class Worker extends Thread {
      def poll(): Option[Job] = tasks.synchronized {
        while (tasks.isEmpty && !terminated && !unimportant) tasks.wait()
        if (unimportant || tasks.isEmpty) {
          // 1) unimportant: simply exit
          // 2) !unimportant yet tasks.isEmpty: this implies that terminated is
          //    true, quit; no need to ask other peers to quit here, since they
          //    will find their way here anyway (no new tasks will be accepted
          //    when terminated is true).
          None
        } else { // terminated and tasks list is not empty and may contain potentially important tasks
          val t = tasks.dequeue()
          if (!terminated || t.priority > importance) {
            Some(t)
          } else {
            unimportant = true
            None
          }
        }
      }

      @tailrec final override def run(): Unit = {
        Thread.sleep(300)
        val name = this.toString
        println(s"Worker thread $name: sleeping for a while to emulate being overloaded ...")
        poll() match {
          case Some(task) => task.job(); run()
          case None => if (!terminated) run()
        }
      }

      this.setDaemon(false)
    }

    def shutdown(): Unit = tasks.synchronized {
      terminated = true
      // To notify all workers up for the new state of being terminated
      tasks.notifyAll()
    }

    def asynchronous(priority: Int)(task: () => Unit): Unit = tasks.synchronized {
      if (terminated)
        throw new UnsupportedOperationException("terminated pool does not accept new job")
      tasks.enqueue(Job(task, priority))
      tasks.notify()
    }

    // There is no reason to keep the worker list.
    Range(0, numThreads) foreach { _ => new Worker().start() }
  }

  // The messages will appear roughly in groups of numThreads ones.
  val p = new PriorityTaskPoolWithMultipleThreads(2, 10)
  Range(0, 20).reverse foreach { i => p.asynchronous(i)(() => println(s"Async job: println $i")) }
  p.shutdown()
  // This will raise no-new-job-allowed exception:
  try {
    Range(0, 1).reverse foreach { i => p.asynchronous(i)(() => println(s"Async job after shutdown(): println $i")) }
  } catch {
    case _: UnsupportedOperationException =>
      println("An UnsupportedOperationException is thrown upon submitting new task after shutdown(), as expected.")
  }
}
