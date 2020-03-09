package org.learningconcurrency
package answers
package ch1

object ch1 extends App {
  // 1.1: Implement a compose() method
  def compose[A, B, C](g: B => C, f: A => B): A => C = a => g(f(a))
  def intToFloat: Int => Float = _.toFloat
  def floatToString: Float => String = _.toString
  val c = compose[Int, Float, String](floatToString, intToFloat)
  c(1)

  // 1.2: Implement a fuse() method
  // stolen from:
  // http://alvinalexander.com/scala/how-to-use-multiple-options-for-loop-comprehension
  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A,B)] = {
    for {
      a1 <- a
      b1 <- b
    } yield (a1, b1)
  }
  fuse(Some(1), None) // Option[(Int, Nothing)] = None
  fuse(Some(1), Some("good")) // Option[(Int, String)] = Some((1,good))

  // 1.3: Implement a check() method
  def check[T](xs: Seq[T])(pred: T => Boolean): Boolean = {
    try {
      for { x <- xs } {
        if (!pred(x))
          return false
      }
      true
    } catch {
      case _ : Throwable => false
    }
  }
  check(0 until 10)(40 / _ > 0) // Boolean = false
  check(1 until 10)(40 / _ > 0) // Boolean = true

  // 1.4: In order to use a class in pattern matching, define it as case class.
  case class Pair[P, Q](first: P, second: Q)
  def isPairQualified[Int, String](that: Pair[Int, String]): Boolean = that match {
    case Pair(1, "good") => true
    case _ => false
  }
  isPairQualified(Pair(1, "good")) // Boolean = true
  isPairQualified(Pair(1, "goody")) // Boolean = false

  // 1.5: Implement a permutations() function that generate a sequence of
  // strings that are lexicographic permutations of the input string.
  // TODO: optimization needed: this function is naive.
  // Only short strings can be feed as input; longer ones will cause memory to
  // exhaust.
  import scala.collection.mutable.ArrayBuffer

  def permutationsIter (arrSorted: ArrayBuffer[String]): ArrayBuffer[String] = {
    if (arrSorted.isEmpty) {
      return ArrayBuffer("")
    } else if (arrSorted.length == 1) {
      return ArrayBuffer(arrSorted.head)
    }
    val ret = ArrayBuffer[String]()
    for (i <- arrSorted.indices) {
      // skip duplicating chars
      if (i == 0 || arrSorted(i) != arrSorted(i - 1)) {
        val n = ArrayBuffer[String]()
        n ++= arrSorted.slice(0, i)
        n ++= arrSorted.slice(i + 1, arrSorted.length)
        for (p <- permutationsIter(n)) {
          ret += arrSorted(i) + p
        }
      }
    }
    ret
  }

  def permutations(s: String): ArrayBuffer[String] = {
    val sortedArray = ArrayBuffer[String]()
    sortedArray ++= s.split("").sortWith(_ < _)
    permutationsIter(sortedArray)
  }

  val s1 = "val ues"
  for (t <- permutations(s1)) { println(t)}
}