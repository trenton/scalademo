package com.runbam.util


import org.junit._
import Assert._
import org.hamcrest.Matchers._
import org.junit.Test
import com.runbam.util.Utils.{parseDecimalDollarsAsPennies}

@Test
class UtilsTest {
  @Test
  def parsePennies: Unit = {
    assertThat(parseDecimalDollarsAsPennies("10"), is(1000))
    assertThat(parseDecimalDollarsAsPennies("10."), is(1000))
    assertThat(parseDecimalDollarsAsPennies("10.0"), is(1000))
    assertThat(parseDecimalDollarsAsPennies("10.00"), is(1000))

    assertThat(parseDecimalDollarsAsPennies("12.2"), is(1220))
    assertThat(parseDecimalDollarsAsPennies("12.02"), is(1202))
    assertThat(parseDecimalDollarsAsPennies("12.20"), is(1220))
    assertThat(parseDecimalDollarsAsPennies("12.34"), is(1234))
    assertThat(parseDecimalDollarsAsPennies("1234"), is(123400))

    assertThat(parseDecimalDollarsAsPennies("0.0"), is(0))
    assertThat(parseDecimalDollarsAsPennies("0.00"), is(0))
    assertThat(parseDecimalDollarsAsPennies("000.00"), is(0))


    try {
      parseDecimalDollarsAsPennies("10.000")
      fail("Should have thrown exception")
    }
    catch {
      case nfe: NumberFormatException => assertTrue(true)
      case _ => fail("Threw unexpected exception")
    }

    try {
      parseDecimalDollarsAsPennies("1.2.3")
      fail("Should have thrown exception")
    }
    catch {
      case nfe: NumberFormatException => assertTrue(true)
      case _ => fail("Threw unexpected exception")
    }
  }

}