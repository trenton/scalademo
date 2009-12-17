package com.runbam.util

import collection.immutable.{SortedMap, TreeMap, RedBlack, TreeSet}
import collection.mutable.ListBuffer
import org.hamcrest.Matcher
import org.junit._
import Assert._
import org.hamcrest.Matchers._
import org.junit.Test

@Test
class RedBlackTreeTest {
  val count: Int = 400000

  @Test
  def testIt = {
    var t = new TreeSet[Int]

    assertThat(t.isEmpty, is(true))
    t += 4

    assertThat(t.isEmpty, is(false))

    assertTrue(t.contains(4))
    t -= 4
    assertFalse(t.contains(4))

  }

  private def isOption[T](opt: Option[T]): Matcher[Option[Any]] = {
    is(opt)
  }

  @Test
  def testRedBlack: Unit = {
    class MyRb[A <% Ordered[A]] private(val size: Int, t: RedBlack[A]#Tree[Unit]) extends RedBlack[A] {
      protected val tree: RedBlack[A]#Tree[Unit] = if (size == 0) Empty else t

      def this() = this (0, null)

      private def newTree(s: Int, t: RedBlack[A]#Tree[Unit]) = new MyRb[A](s, t)

      def isSmaller(x: A, y: A) = x < y

      def +(elem: A): MyRb[A] = {
        val newsize = if (tree.lookup(elem).isEmpty) size + 1 else size
        newTree(newsize, tree.update(elem, ()))
      }

      def -(elem: A): MyRb[A] =
        if (tree.lookup(elem).isEmpty) this
        else newTree(size - 1, tree.delete(elem))

      def contains(elem: A): Boolean = !tree.lookup(elem).isEmpty

      def first: A = tree.first

      def last: A = tree.last
    }

    var t = new MyRb[Int]

    val rand = new scala.util.Random
    var entries = new ListBuffer[Int]

    println("building")


    (0 until count).foreach(i => {
      val value: Int = rand.nextInt(Integer.MAX_VALUE)

      // do not allow dups for purposes of this test
      if (!t.contains(value)) {
        entries += value
        t += value
      }
    })

    println("testing")

    println("first: %d, last: %d".format(t.first, t.last))

    val start = System.currentTimeMillis

    entries.foreach(x => {
      assertThat(t.contains(x), is(true))

      t -= x

      assertThat(t.contains(x), is(false))
    })

    val end = System.currentTimeMillis
    println("et: %.1f sec".format((end - start) / 1000f))
  }

  @Test
  def testTreeSet: Unit = {
    var t = new TreeSet[Int]

    val rand = new scala.util.Random
    var entries = new ListBuffer[Int]

    println("building")

    (0 until count).foreach(i => {
      val value: Int = rand.nextInt(Integer.MAX_VALUE)

      // do not allow dups for purposes of this test
      if (!t.contains(value)) {
        entries += value
        t = t.insert(value)
      }
    })

    println("testing")

    val start = System.currentTimeMillis

    entries.foreach(x => {
      assertThat(t.contains(x), is(true))

      t -= x

      assertThat(t.contains(x), is(false))
    })

    val end = System.currentTimeMillis
    println("et: %.1f sec".format((end - start) / 1000f))
  }
}