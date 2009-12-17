package com.runbam.util

import collection.immutable.{TreeSet}
import org.hamcrest.Matcher
import org.junit._
import Assert._
import org.hamcrest.Matchers._
import org.junit.Test

@Test
class ListMapTest {
  @Test
  def testTreeMap = {
    var t = new ListMap[Int, String]

    assertFalse(t.contains(1))
    assertThat(t.get(1), isOption(None))

    t += (1, "one")

    assertTrue(t.contains(1))
    assertThat(t.get(1), isOption(Some(List("one"))))

    t += (1, "une")
    t += (1, "uno")

    assertThat(t.get(1), isOption(Some(List("one", "une", "uno"))))

    val keySet: Set[Int] = new TreeSet[Int] ++ t.keys
    assertTrue(keySet.contains(1))

    // :TODO: need to actually test here
    t.values.flatMap(_.elements).foreach(print _)

    t -= 1
    assertThat(t.get(1), isOption(None))
  }

  @Test
  def testTreeMapGenerics = {
    var t = new ListMap[Long, Double]

    t += (1, 1.0)
    t += (1, 1.5)
    t += (1, 1.9)

    t += (2, 2.2)
    t += (2, 2.4)

    assertThat(t.get(1), isOption(Some(List(1.0, 1.5, 1.9))))
    assertThat(t.get(2), isOption(Some(List(2.2, 2.4))))

    t -= 1
    assertThat(t.get(1), isOption(None))

    t -= 2
    assertThat(t.get(2), isOption(None))
  }

  private def isOption[T](opt: Option[T]): Matcher[Option[Any]] = {
    is(opt)
  }
}
