package advent

object Day3 {

  def main(args: Array[String]): Unit = {
    import File._

    def hasTree(line: String, pos: Int): Boolean = line(pos % line.length) == '#'

    println({
      read("day3") match {
        case _ :: all => {
          for {
            (right, down) <- List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
          } yield for {
            line <- (down - 1) until all.length by down
            if (hasTree(all(line), ((line + 1) / down) * right))
          } yield line
        }
      }
    }.map(_.length).map(BigInt(_)).product + " is the answer")
  }
}
