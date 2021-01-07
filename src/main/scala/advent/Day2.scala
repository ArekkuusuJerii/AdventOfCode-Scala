package advent

import scala.util.matching.Regex

object Day2 {

  val regex: Regex = "(\\d+-\\d+) ([a-z]): ([a-z]*)".r

  def main(args: Array[String]): Unit = {
    import File._

    def count(s: String, c: Char): Option[Int] = {
      Some({ for {c1 <- s if c1 == c} yield c1 }.length)
    }

    def isValid(s: String): Boolean = {
      Some(regex.pattern.matcher(s))
        .filter(_.matches())
        .map(m => (m.group(1).split('-').map(_.toInt), m.group(2)(0), m.group(3)))
        //.flatMap { case (a, b, c) => count(c, b).filter(t => t >= a(0) && t <= a(1)) } // Part one
        //.flatMap { case (a, b, c) => if((c(a(0) - 1) == b) != (c(a(1) - 1) == b)) Some(c) else None } // Part two
        .nonEmpty
    }

    println ({ for (line <- read("day2") if isValid(line)) yield line }.length + " inputs are valid")
  }
}
