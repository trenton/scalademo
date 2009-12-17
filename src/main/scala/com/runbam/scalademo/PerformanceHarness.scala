package com.runbam.scalademo

object PerformanceHarness {
  def main(args: Array[String]) = {
    val rand = new scala.util.Random
    val count: Int = Integer.parseInt(args(0))

    println("building")

    // list of things to be added to data structure
    val entries = (0 until count).map(i => {
      rand.nextInt(Integer.MAX_VALUE)
    })

    // list of things to be removed from data structure
    val entriesToDelete: List[Int] = everyOther(true, entries, List())

    // data structure being tested
    var guineaPig: Set[Int] = new scala.collection.immutable.HashSet[Int]
//    var guineaPig = new scala.collection.immutable.TreeSet[Int]
//    var guineaPig = new scala.collection.mutable.HashSet[Int]
//    var guineaPig = new com.runbam.util.BinarySearchTree

    println("testing " + guineaPig.getClass.getName)
    val start = System.currentTimeMillis

    // add everything
    entries.foreach(e => {
      guineaPig = guineaPig + e
//            guineaPig += e
    })

    // delete some things
    entriesToDelete.foreach(e => {
      guineaPig = guineaPig - e
//            guineaPig -= e
    })

    // delete the rest... with lots of misses
    entries.foreach(e => {
      guineaPig = guineaPig - e
//            guineaPig -= e
    })

    val end = System.currentTimeMillis

    val et: Float = (end - start) / 1000f
    print("%.1f sec, %.1f ops/sec\n".format(et, entries.size / et))
  }

  def everyOther(b: Boolean, xs: RandomAccessSeq[Int], acc: List[Int]): List[Int] = {
    xs.isEmpty match {
      case true => acc
      case false if b => everyOther(false, xs.drop(1), xs.first :: acc)
      case _ => everyOther(true, xs.drop(1), acc)
    }
  }

}