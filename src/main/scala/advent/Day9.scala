package advent

object Day9 {

  def main(args: Array[String]): Unit = {
    import File._

    def findPair(array: List[Long], num: Long) = {
      @scala.annotation.tailrec
      def recursiveFindPair(l: Int, r: Int): Option[(Long, Long)] = {
        if(l > r) {
          None
        }
        else if(array(l) + array(r) == num) {
          Some((l, r))
        }
        else if(array(l) + array(r) < num) {
          recursiveFindPair(l + 1, r)
        }
        else {
          recursiveFindPair(l, r - 1)
        }
      }

      recursiveFindPair(0, array.length - 1)
    }

    @scala.annotation.tailrec
    def findMatch(array: List[Long]): Some[Long] = {
      array(25) match {
        case i => if(findPair(array.take(25).sorted, i).nonEmpty) findMatch(array.drop(1)) else Some(i)
      }
    }

    @scala.annotation.tailrec
    def findList(array: List[Long], num: Long)(size: Int = 2): List[Long] = {
      array.take(size) match {
        case Nil => List()
        case set => {
          set.sum match {
            case `num` => set.sorted
            case sum if(sum < num) => findList(array, num)(size + 1)
            case _ => findList(array.drop(1), num)()
          }
        }
      }
    }

    println({ findMatch(read("day9").iterator.map(_.toLong).toList).get } + " is the answer") //Part 1
    println({ read("day9").iterator.map(_.toLong).toList match { case array => findMatch(array) match { case Some(n) => findList(array, n)() } } } + " is the answer") //Part 2
  }
}
