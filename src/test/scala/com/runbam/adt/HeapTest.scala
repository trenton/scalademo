package com.runbam.adt


import adt.heap.{FibonacciHeap, Heap, BinomialHeap}
import org.hamcrest.Matcher
import org.junit._
import Assert._
import org.hamcrest.Matchers._
import org.junit.Test
import HeapTest._

class HeapTest {
  @Test
  def testBinomialHeap() {
    val h = new BinomialHeap[Int](0)
    testHeapHelper(h)
  }

  @Test
  def testFibonacciHeap() {
    val h = new FibonacciHeap[Int](0)
    testHeapHelper(h)
  }

  @Ignore
  def perfTestHeap() {
    val r = new scala.util.Random
    val h = new BinomialHeap[Int](0)

    var entryCount = 1000000

    (0 until entryCount).foreach(x => h += r.nextInt(Integer.MAX_VALUE))
    assertThat(h.size, is(entryCount))
    (0 until entryCount).foreach(x => {
      if(x % 1000 == 0) {
        println(h.size + " remaining")
      }
      h.extractMin
    })
    assertThat(h.size, is(0))
  }

  @Ignore
  def testPriorityQueue {
    // https://lampsvn.epfl.ch/trac/scala/ticket/1594
    val q = new scala.collection.mutable.PriorityQueue[Int] {
      override def length = super.length // - 1
    }

    //    assertThat(q.isEmpty, is(true))

    //    assertThat(q.size, is(0))
    //    assertThat(q.length, is(0))

    q += 2
    q += 1

    //    println(q.toString())
    //    println(q.toString())
    //    println(q.first)

    assertThat(q.first, is(2))
  }
}

object HeapTest {
  // if Matcher[Option[Any]] is returned, we get compile errors
  def isOption[T](opt: Option[T]): Matcher[Option[Any]] = {
    is(opt)
  }

  def testHeapHelper(h: Heap[Int]) {
    h += 1
    h += 3
    h += 2

    assertThat(h.size, is(3))

    // :TODO: blog this
    //    lazy val matcher: Matcher[Option[Int]] = is(Some(1))
    assertThat(h.minimum, isOption(Some(1)))
    assertThat(h.extractMin, isOption(Some(1)))
    assertThat(h.minimum, isOption(Some(2)))
    assertThat(h.extractMin, isOption(Some(2)))
    assertThat(h.extractMin, isOption(Some(3)))
    assertThat(h.extractMin, isOption(None))

    h += 10
    h += 10
    h += 100

    assertThat(h.size, is(3))

    assertThat(h.minimum, isOption(Some(10)))
    h -= 10
    assertThat(h.minimum, isOption(Some(10)))
    h -= 10
    assertThat(h.minimum, isOption(Some(100)))

    assertThat(h.size, is(1))
  }
}