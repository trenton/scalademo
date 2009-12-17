package com.runbam.util

import java.lang.Integer.parseInt

object Utils {
  def parseDecimalDollarsAsPennies(in: String): Int = {
    // position of decimal determines how much to multiply by

    val dotAt = in.indexOf(".")

    val dotFromEnd = dotAt match {
      case -1 => -1
      case _ => in.length - dotAt - 1
    }

    dotFromEnd match {
    // no decimal or at the end... no pennies, so mult by 100
      case -1 => parseInt(in) * 100
      // decimal right at the end
      case 0 => parseInt(in.substring(0, dotAt)) * 100
      // tens specified... so turn them into 10 pennies
      case 1 => parseInt(in.substring(0, dotAt)) * 100 + parseInt(in.substring(dotAt + 1, in.length)) * 10
      // two away... already in pennies, just get rid of the dot
      case 2 => parseInt(in.substring(0, dotAt)) * 100 + parseInt(in.substring(dotAt + 1, in.length))
      // everything else is junk
      case _ => throw new NumberFormatException(in)
    }
  }
}