package org.learningconcurrency
package answers
package ch3

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent._

// ex3-2: TreiberStack
// Key to being 'lock-free and not susceptible to the ABA problem':
// 1) The atomic reference with compare-and-swap idiom is used.
// 2) The push/pop operations are tail-recursive.
// [Upon seeing the ref. answer after coding is done]
// Notes:
// 1. This impl. is unnecessarily complex. The bottom nodes and value nodes
//    are sufficiently similar: they can be distinguished by setting next
//    to Null or not; we don't need two dedicated classes and distinguish
//    by case-clause. That way, the data can be a plain List[T].

object Ex2 extends App {

  class TreiberStack[T] {
    sealed trait Node
    class Bottom extends Node
    class ValueNode(val value: T, var next: AtomicReference[Node]) extends Node

    private val data = new AtomicReference[Node](new Bottom())

    @tailrec final
    def push(x: T): Unit = {
      val oldTop = data.get
      // TODO don't know why here
      // If 'data' is used instead of 'new AtomicReference[Node](oldTop)'
      // here, pop() will always return the value at top (the stack never
      // changes)!
      val newTop = new ValueNode(x, new AtomicReference[Node](oldTop))
      if (!data.compareAndSet(oldTop, newTop))
        push(x)
    }

    @tailrec final
    def pop(): T = {
      val oldTop = data.get
      oldTop match {
        case b: Bottom => throw new RuntimeException("pop(): the stack is empty")
        case n: ValueNode =>
          val newTop = n.next.get
          val v = n.value
          if (data.compareAndSet(oldTop, newTop))
            v
          else
            pop()
      }
    }
  }

  val stack = new TreiberStack[Int]()
  val ectx = ExecutionContext.global
  ectx.execute(new Runnable {
    override def run(): Unit = {
      for (i <- 1 to 10) {
        Thread.sleep(scala.util.Random.nextInt(100))
        println(s"push() to stack: value $i")
        stack.push(i)
      }
    }
  })
  Thread.sleep(100)
  while (true) {
    Thread.sleep(scala.util.Random.nextInt(100))
    println(s"pop() from stack: value ${stack.pop()}")
  }
}
