package advent

object Day7 {

  def main(args: Array[String]): Unit = {
    import File._

    def find(map: Map[String, Seq[String]], current: String)(lookup: String): Boolean = {
      map(current).distinct match {
        case seq@_ => if(seq.contains(lookup)) true else seq.exists(find(map, _) {lookup})
        case Nil => false
      }
    }

    def count(map: Map[String, Seq[String]], current: String): Int = {
      map(current) match {
        case seq@_ => seq.length + seq.map(count(map, _)).sum
        case Nil => 0
      }
    }

    def parse(string: String): (String, Seq[String]) = {
      def deeper(string: String) = {
        Some("(\\d*) (.*) bags?".r.pattern.matcher(string.trim)).filter(_.matches()) match {
          case Some(m) => for { _ <- 0 until m.group(1).toInt } yield m.group(2)
          case None => Nil
        }
      }

      Some("(.*) bags contain (.*)\\.".r.pattern.matcher(string)).filter(_.matches()) match {
        case Some(m) => (m.group(1) -> m.group(2).split(',').filterNot(_.isEmpty).flatMap(deeper))
      }
    }

    println({ read("day7").iterator.foldLeft(Map[String, Seq[String]]()) { (l, r) => l + parse(r) } match { case map@_ => map.keys.iterator.filter(u => find(map, u) {"shiny gold"}) } }.length + " is the answer") //Part 1
    println({ read("day7").iterator.foldLeft(Map[String, Seq[String]]()) { (l, r) => l + parse(r) } match { case map@_ => count(map, "shiny gold") } } + " is the answer") //Part 2
  }
}
