package ru.ifmo.genome.util

class ConsoleProgress(label: String, width: Int, labelColor: Option[String] = None, progressColor: Option[String] = None, percentColor: Option[String] = None) {
  var complete = false
  var lastPercent = -1L

  def apply(progress: Double) {
    val percent = math.round(progress * 1000.0)
    if (!complete && percent != lastPercent) {
      //print the new line
      val barWidth = (width * progress).toInt
      val barRemainder = width - barWidth

      val labelOutput = label + ": "
      val progressBar = "[" + ("=" * barWidth) + (" " * barRemainder) + "] "


      labelColor foreach print
      print(labelOutput)
      if (labelColor.isDefined) print(Console.RESET)

      progressColor foreach print
      print(progressBar)
      if (progressColor.isDefined) print(Console.RESET)

      percentColor foreach print
      print(percent / 10d + "%")
      if (percentColor.isDefined) print(Console.RESET)

      //deal with 100% progress
      if (percent == 1000) {
        complete = true
        println()
      } else {
        print("\r")
      }
      lastPercent = percent
    }
  }

  def done() {
    apply(1d)
    assert(complete)
  }
}