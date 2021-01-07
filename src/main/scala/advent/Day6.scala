package advent

object Day6 {

  def main(args: Array[String]): Unit = {
    import File._

    def concat(last: String, next: String) = {
      if (next.isEmpty) last + " "
      else last + next
    }

    def reduce(t: (Int, String), next: String) = {
      def process(string: String): Int = {
        string split ' ' filterNot(_.isEmpty) match {
          case all@Array(one, _*) => (all foldLeft one) { (l, r) => l filter (r contains _) }.distinct.length
        }
      }

      t match {
        case (total, prev) => {
          if (next.isEmpty) (total + process(prev), "")
          else (total, s"${prev} ${next}")
        }
      }
    }

    println({ (read("day6").iterator.foldLeft("") { concat } split ' ' map (_.distinct) map (_.length)).sum } + " is the answer") //Part 1
    println({ read("day6").iterator.foldLeft((0,"")) { reduce } match { case (l, _) => l } } + " is the answer") //Part 2
  }
}
