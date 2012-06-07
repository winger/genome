package ru.ifmo.genome.scripts

import java.io.{BufferedReader, InputStreamReader}
import java.util.regex.Pattern


/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

object N50 extends App {
  val in = new BufferedReader(new InputStreamReader(System.in))
  val line = in.readLine()
  val pattern = Pattern.compile("[^\\d](\\d+) -> (\\d+),")
  val matcher = pattern.matcher(line)
  var pairs = Seq[(Int, Int)]()
  while (matcher.find()) {
    val length = matcher.group(1).toInt
    val count = matcher.group(2).toInt
    pairs :+= (length, count)
  }
  pairs = pairs.sorted
  val total = pairs.map(p => p._1 * p._2).sum
  var sum = 0
  for (p <- pairs) {
    if (2 * sum < total && 2 * (sum + p._1 * p._2) >= total) {
      println(p)
    }
    sum += p._1 * p._2
  }
  println(pairs)
  println(pairs.filter(_._1 >= 100).map(_._2).sum)
}
