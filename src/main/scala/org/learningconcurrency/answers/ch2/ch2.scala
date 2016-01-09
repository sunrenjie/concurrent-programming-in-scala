package org.learningconcurrency
package answers
package ch2

object ch2 extends App {
  //common
  def thread(body: => Unit): Thread = {
    val t = new Thread {
      override def run() = body
    }
    t.start()
    t
  }


  // 2.1
  def parallel[A, B](a: () => A, b: () => B): (A, B) = {
    // Defined as Option[] so that they could be uninitialized; stolen from:
    // http://stackoverflow.com/questions/2440134/is-this-the-proper-way-to-initialize-null-references-in-scala
    // The textbook code use another approach:
    // var aVal: A = null.asInstanceOf[A]
    var ra = None: Option[A]
    var rb = None: Option[B]
    val ta = new Thread {
      override def run() = {
        ra = Some(a())
      }
    }
    ta.start()
    val tb = new Thread {
      override def run() = {
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


  // 2.2
  // This implementation is inefficient: setting the thread as daemon and
  // placing the endless loop within is much better
  def periodically(duration: Long, b: => Unit): Unit = {
    while (true) {
      val t = new Thread {
        override def run() = {
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
    override def run() = {
      Thread.sleep(5000)
      val v = x.getWait
      println("thread getWait() value " + v.toString)
    }
  }
  t.start()
  x.putWait(3)


  // 2.6 SyncVar class that hold at most n values
  class SyncQueue[T](capacity: Int) {

    import scala.collection.mutable

    val data = mutable.Queue[T]()

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

    def getWait(): T = this.synchronized {
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
    override def run() = {
      var x = q.getWait()
      println("thread: get($x) is done")
      x = q.getWait()
      println("thread: get($x) is done")
    }
  }
  th.start()
  Thread.sleep(5000)

  q.put(1)
  q.put(2)
  q.put(3)
  q.put(4)
  println("thread of 4 put() is done")


  // 2.7
  // TODO incorrect solution
  /*

  The sendAll() is atomic; we've got to lock all accounts in the first place.
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
  object SynchronizedProtectedUid extends App {

    var uidCount = 0

    def getUniqueId() = this.synchronized {
      val freshUid = uidCount + 1
      uidCount = freshUid
      freshUid
    }

    def printUniqueIds(n: Int): Unit = {
      val uids = for (i <- 0 until n) yield getUniqueId()
      println(s"Generated uids: $uids")
    }

    val t = thread {
      printUniqueIds(5)
    }
    printUniqueIds(5)
    t.join()

  }


  object SynchronizedDeadlock extends App {

    import SynchronizedProtectedUid._

    class Account(val name: String, var money: Int) {
      val uid = getUniqueId()
    }

    def sendAll(accounts: Set[Account], target: Account): Unit = target.synchronized {
      accounts foreach { a => assert(a.uid != target.uid) }
      accounts foreach {
        a => {
          a.synchronized {
            target.money += a.money
            a.money = 0
          }
        }
      }
    }

    val a = new Account("Jill", 1000)
    val b = new Account("Jack", 2000)
    val c = new Account("Ellis", 3000)
    val d = new Account("Bob", 3000)
    val e = new Account("Neil", 3000)
    val f = new Account("Tom", 3000)
    val set = Set(a, b, c, d, e)
    sendAll(set, f)
    println(s"f = ${f.money}")
  }


  // 2.8
  import scala.annotation.tailrec
  import scala.collection.mutable

  object PriorityTaskPool extends App {

    // PriorityQueue code stolen from text book code and from:
    // http://stackoverflow.com/questions/789250/scala-is-there-a-way-to-use-priorityqueue-like-i-would-in-java

    class Job(task: => Unit, pri: Int) {
      val job: () => Unit = () => task
      val priority = pri
    }

    implicit def orderedJob(j: Job): Ordered[Job] = new Ordered[Job] {
      def compare(other: Job) = j.priority.compare(other.priority)
    }

    val tasks = new mutable.PriorityQueue[Job]()

    object Worker extends Thread {
      var terminated = false

      def poll(): Option[Job] = tasks.synchronized {
        while (tasks.isEmpty && !terminated) tasks.wait()
        if (!terminated) Some(tasks.dequeue()) else None
      }

      @tailrec override def run() = {
        // being extremely lazy to make the asynchronous nature obvious
        Thread.sleep(5000)
        poll() match {
          case Some(task) => task.job(); run()
          case None =>
        }
      }

      def shutdown() = tasks.synchronized {
        terminated = true
        tasks.notify()
      }
    }

    def asynchronous(priority: Int)(task: => Unit): Unit = tasks.synchronized {
      tasks.enqueue(new Job(task, priority))
      tasks.notify()
    }

    Worker.start()
    Range(0, 9).reverse foreach { i => asynchronous(i)(println(s"println $i")) }
  }

  PriorityTaskPool.main(Array[String]())


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

    class Job(task: => Unit, pri: Int) {
      val job: () => Unit = () => task
      val priority = pri
    }

    implicit def orderedJob(j: Job): Ordered[Job] = new Ordered[Job] {
      def compare(other: Job) = j.priority.compare(other.priority)
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
        } else {
          val t = tasks.dequeue()
          if (!terminated || t.priority > importance) {
            Some(t)
          } else {
            unimportant = true
            None
          }
        }
      }

      @tailrec final override def run() = {
        Thread.sleep(5000)
        poll() match {
          case Some(task) =>
            if (task.priority > importance)
              task.job()
            run()
          case None =>
        }
      }
    }

    def shutdown() = tasks.synchronized {
      terminated = true
      tasks.notify()
    }

    def asynchronous(priority: Int)(task: => Unit): Unit = tasks.synchronized {
      if (terminated)
        throw new UnsupportedOperationException("terminated pool does not accept new job")
      tasks.enqueue(new Job(task, priority))
      tasks.notify()
    }

    // There is no reason to keep the worker list.
    Range(0, numThreads) foreach { _ => new Worker().start() }
  }

  // The messages will appear roughly in groups of numThreads ones.
  val p = new PriorityTaskPoolWithMultipleThreads(3, 20)
  Range(0, 20).reverse foreach { i => p.asynchronous(i)(println(s"println $i")) }
  p.shutdown()
  // This will raise no-new-job-allowed exception:
  Range(0, 1).reverse foreach { i => p.asynchronous(i)(println(s"println $i")) }
}