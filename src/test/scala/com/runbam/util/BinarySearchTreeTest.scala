package com.runbam.util

import collection.mutable.ListBuffer
import org.junit._
import Assert._
import org.hamcrest.Matchers._
import org.junit.Test

@Test
class BinarySearchTreeTest {
  @Test
  def testIt = {
    val t = new BinarySearchTree

    assertThat(t.isEmpty, is(true))
    assertThat(t.size, is(0))
    assertThat(t.search(3), is(false))

    t.insert(3)
    t.insert(5)
    t.insert(1)

    assertThat(t.isEmpty, is(false))
    assertThat(t.size, is(3))
    assertThat(t.search(3), is(true))

    t.insert(55)
    t.insert(0)
    t.insert(0)

    assertThat(t.search(0), is(true))

    println(t.iterate.mkString(","))
  }

  @Test
  def testFindMin: Unit = {
    val t = new BinarySearchTree

    try {
      t.findMin
      fail("findMin on empy tree shoudl throw exception")
    }
    catch {
      case e: NoSuchElementException => assertTrue(true)
      case _ => fail("Caught unknown exception")
    }

    t.insert(5)
    assertThat(t.findMin, is(5))
    t.insert(3)
    assertThat(t.findMin, is(3))
  }

  @Test
  def testFindMax: Unit = {
    val t = new BinarySearchTree

    try {
      t.findMin
      fail("findMax on empy tree shoudl throw exception")
    }
    catch {
      case e: NoSuchElementException => assertTrue(true)
      case _ => fail("Caught unknown exception")
    }

    t.insert(50)
    assertThat(t.findMax, is(50))
    t.insert(300)
    assertThat(t.findMax, is(300))
  }

  @Test
  def testInsertLots: Unit = {
    val t = new BinarySearchTree

    val rand = new scala.util.Random
    (0 until 300000).foreach(x => t.insert(rand.nextInt(Integer.MAX_VALUE)))

    validateTree(t)
  }

  @Test
  def testSearch = {
    val t = new BinarySearchTree
    assertThat(t.search(3), is(false))
    t.insert(3)
    assertThat(t.search(3), is(true))
    t.insert(5)
    assertThat(t.search(5), is(true))
    t.insert(0)
    assertThat(t.search(0), is(true))
    t.insert(23141234)
    assertThat(t.search(23141234), is(true))

  }

  @Test
  def testDeleteRoot = {
    val t = new BinarySearchTree
    assertTrue(t.isEmpty)
    t.insert(0)
    t.insert(1)
    assertFalse(t.isEmpty)
    t.delete(0)
    assertFalse(t.isEmpty)
    t.delete(1)
    assertTrue(t.isEmpty)
  }

  @Test
  def testDeleteLeaf = {
    val t = new BinarySearchTree

    assertThat(t.size, is(0))
    assertThat(t.search(3), is(false))

    t.insert((3))
    assertFalse(t.isEmpty)
    assertThat(t.size, is(1))
    assertThat(t.search(3), is(true))

    t.delete(3)
    assertThat(t.search(3), is(false))
    assertThat(t.size, is(0))
    assertTrue(t.isEmpty)

    t.insert(5)
    t.insert(3)
    t.insert(9)

    // delete leaf
    t.delete(3)
    validateTree(t)

    assertThat(t.search(3), is(false))
    assertThat(t.search(5), is(true))
    assertThat(t.search(9), is(true))

    t.delete(9)
    validateTree(t)
    assertThat(t.search(9), is(false))
  }

  @Test
  def testDeleteOneChildNode = {
    val t = new BinarySearchTree

    t.insert(6)
    t.insert(8)
    t.insert(2)
    t.insert(4)
    t.insert(1)
    t.insert(3)

    t.delete(4)
    validateTree(t)

    assertThat(t.search(4), is(false))
  }

  @Test
  def testDeleteTwoChildNode = {
    val t = new BinarySearchTree

    t.insert(10)
    t.insert(100)
    t.insert(80)
    t.insert(120)

    t.delete(100)
    assertThat(t.search(100), is(false))
    validateTree(t)
  }

  @Ignore
  def testasdfasdf = {
    val t = new BinarySearchTree
    val rand = new scala.util.Random
    val count: Int = 200000

    t.insert(-1)

    var entries = new ListBuffer[Int]
    (0 until count).foreach(x => {
      val value: Int = rand.nextInt(Integer.MAX_VALUE)
      entries += value
      t.insert(10)
    })

    entries.foreach(i => {
      t.delete(10)
    })

    assertThat(t.size, is(1))
  }

  @Test
  def testDeleteNotThere: Unit = {
    val t = new BinarySearchTree

    t += 1

    assertThat(t.search(1), is(true))
    assertThat(t.size, is(1))

    t -= 0

    assertThat(t.search(1), is(true))
    assertThat(t.size, is(1))

    t -= 1

    assertThat(t.search(1), is(false))
    assertThat(t.size, is(0))
  }

  @Test
  def testDeleteLots: Unit = {
    val t = new BinarySearchTree

    val rand = new scala.util.Random
    val count: Int = 400000
    val entries = new ListBuffer[Int]

    println("building")

    (0 until count).foreach(i => {
      val value: Int = rand.nextInt(Integer.MAX_VALUE)

      // do not allow dups for purposes of this test
      if(! t.search(value)) {
        entries += value
        t.insert(value)
      }
    })

    println("testing")

    val start = System.currentTimeMillis

    var i = 0
    entries.toList.zipWithIndex.foreach(e => {
      val value = e._1
      val index = e._2

      assertThat(t.search(value), is(true))

      // perform periodic validations (ten times total, regardless of input size)
      if (index % (count / 10) == 0) {
        println("validating tree")
        validateTree(t)
      }

      t.delete(value)

      assertThat(t.search(value), is(false))
    })

    val end = System.currentTimeMillis
    println("*%.1f* sec".format((end - start) / 1000f))
  }

  @Test
  def testNodeDegree: Unit = {
    assertThat(new Node(0, None, None).degree, is(0))
    assertThat(new Node(0, Some(new Node(1, None, None)), None).degree, is(1))
    assertThat(new Node(0, None, Some(new Node(1, None, None))).degree, is(1))
    assertThat(new Node(0, Some(new Node(1, None, None)), Some(new Node(1, None, None))).degree, is(2))
  }

  def validateTree(t: BinarySearchTree): Unit = {
    t.iterate.foldLeft(t.findMin)((previous, current) => {
      if (current < previous) throw new RuntimeException("node out of order: %d < %d".format(current, previous))
      else current
    })
  }
}