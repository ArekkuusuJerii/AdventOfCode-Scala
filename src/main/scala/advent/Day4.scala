package advent

object Day4 {

  def main(args: Array[String]): Unit = {
    import File._

    def validate(data: String): Boolean = {
      data.split(':') match {
        case Array(a, b) => a match {
          case "byr" => "^(19[2-9][0-9]|200[0-2])$".r.pattern.matcher(b).matches()
          case "iyr" => "^(201[0-9]|2020)$".r.pattern.matcher(b).matches()
          case "eyr" => "^(202[0-9]|2030)$".r.pattern.matcher(b).matches()
          case "hgt" => "^(((1[5-8][0-9]|19[0-3])cm)|((59|6[0-9]|7[0-6])in))$".r.pattern.matcher(b).matches()
          case "hcl" => "^#([0-9a-f]{6})$".r.pattern.matcher(b).matches()
          case "ecl" => "^(amb|blu|brn|gry|grn|hzl|oth)$".r.pattern.matcher(b).matches()
          case "pid" => "^[0-9]{9}$".r.pattern.matcher(b).matches()
        }
      }
    }

    def process(string: String): Int = {
      if(string.split(' ').filterNot(_.isEmpty).filterNot(_.take(3).equals("cid")).count(validate) >= 7) 1 else 0
    }

    def reduce(t: (Int, String), next: String) = {
      t match {
        case (total, prev) => {
          if(next.isEmpty) (total + process(prev), "")
          else (total, s"${prev} ${next}")
        }
      }
    }

    println({ read("day4").iterator.foldLeft((0, ""))((t, next) => reduce(t, next)) match { case (i, _) => i} } + " is the answer")
  }
}
