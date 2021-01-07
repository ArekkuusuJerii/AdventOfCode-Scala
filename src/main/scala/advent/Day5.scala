package advent

object Day5 {

  def main(args: Array[String]): Unit = {
    import File._

    def floor(a: Int, b: Int) = ((b - a) / 2).floor.toInt
    def ceil(a: Int, b: Int) = ((b - a) / 2).ceil.toInt

    @scala.annotation.tailrec
    def findCol(t: (Int, Int), s: String) : Int = {
      if(s.length == 1) s match { case "L" => t._1; case _ => t._2}
      else findCol(s.take(1) match { case "L" => (t._1, t._1 + floor(t._1, t._2)); case _ => (t._2 - ceil(t._1, t._2), t._2)}, s.drop(1))
    }

    @scala.annotation.tailrec
    def findRow(t: (Int, Int), s: String): Int = {
      if(s.length == 1) { s match { case "F" => t._1; case _ => t._2} }
      else findRow(s.take(1) match { case "F" => (t._1, t._1 + floor(t._1, t._2)); case _ => (t._2 - ceil(t._1, t._2), t._2)}, s.drop(1))
    }

    def findPass(line: String): Int = {
      findRow((0, 127), line.take(7)) * 8 + findCol((0,7), line.drop(7))
    }

    def findCode(line: String): (Int, Int) = {
      (findRow((0, 127), line.take(7)), findCol((0,7), line.drop(7)))
    }

    lazy val totalCols = (1 to 7).sum
    lazy val array = Array.fill[(Int, Int)](127) { (0,0) }
    lazy val fold = (a : Array[(Int, Int)], b: (Int, Int)) => b match { case (l, r) => a.updated(b._1, (l, {a(l) match { case (_,t) => t }} + r)) }
    lazy val empty = (b: (Int, Int)) => b match { case (_, b) => b != 0 }
    lazy val missingCol = (b: (Int, Int)) => b match { case (_, b) => b < totalCols }
    lazy val toPass = (b: (Int, Int)) => b match { case (a, b) => a * 8 + (totalCols - b) }

    println({ for { line <- read("day5") } yield findPass(line) }.iterator.max + " is the answer") // Part 1
    println({ for { line <- read("day5") } yield findCode(line) }.iterator.foldLeft(array) { (a, b) => fold(a, b) }.filter(empty).find(missingCol).map(toPass) + " is the answer") // Part 2

    // woah...
    println({ for { line <- read("day5") } yield line.map(c => if((c & 4) == 0) '1' else '0') }.iterator.map(Integer.parseInt(_, 2)).max + " is the short answer")
  }
}
