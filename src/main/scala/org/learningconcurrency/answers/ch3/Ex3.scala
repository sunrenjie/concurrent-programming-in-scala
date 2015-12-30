package org.learningconcurrency
package answers
package ch3

// ex-3.3 (and 3.4): ConcurrentSortedList
// [Upon seeing the ref. answer after coding is done]
// Notes:
// 1. This impl. is still unnecessarily complex. Only one class is actually
//    required.
// Changes:
// 1. The iterator class is defined as anonymous, instead of being given a
//    name.
object Ex3 extends App {

  import java.util.concurrent.atomic.AtomicReference
  import scala.annotation.tailrec
  import scala.concurrent._

  class ConcurrentSortedList[T] (implicit val ord: Ordering[T]) {
    sealed trait Node
    class TerminalNode extends Node
    class ValueNode(val value: T, var next: AtomicReference[Node]) extends Node

    private val data = new AtomicReference[Node](new TerminalNode())

    @tailrec final
    def add(x: T, ref: AtomicReference[Node]): Unit = {
      val oldTop = ref.get
      oldTop match {
        case v: ValueNode if ord.compare(x, v.value) > 0 =>
          add(x, v.next)
        case _ => // t: TerminalNode or v: ValueNode if ord.compare(x, v.value) <= 0
          val newTop = new ValueNode(x, new AtomicReference[Node](oldTop))
          if (!ref.compareAndSet(oldTop, newTop))
            add(x, ref)
      }
    }

    def add(x: T): Unit = {
      add(x, data)
    }

    def iterator: Iterator[T] = new Iterator[T] {
      var current = data

      override def hasNext(): Boolean = {
        current.get match {
          case t: TerminalNode => false
          case v: ValueNode => true
        }
      }

      override def next(): T = {
        current.get match {
          case t: TerminalNode => throw new RuntimeException("next(): list is empty")
          case v: ValueNode =>
            val x = v.value
            current = v.next
            x
        }
      }
    }

  }

  val l = new ConcurrentSortedList[Int]()
  val ectx = ExecutionContext.global
  ectx.execute(new Runnable {
    override def run(): Unit = {
      for (i <- 1 to 100) {
        Thread.sleep(scala.util.Random.nextInt(100))
        println(s"add to list: value $i")
        l.add(i)
      }
    }
  })
  Thread.sleep(100)
  while (true) {
    Thread.sleep(scala.util.Random.nextInt(100))
    val iter = l.iterator
    var items: List[Int] = Nil
    while (iter.hasNext) {
      items :+= iter.next()
    }
    println(s"currently at list: value $items")
  }
}
