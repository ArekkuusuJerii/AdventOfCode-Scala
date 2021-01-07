package advent

object Day11 {

  sealed abstract class Space
  case object Floor extends Space
  case class Occupied(x: Int, y: Int) extends Space
  case class Available(x: Int, y: Int) extends Space

  def main(args: Array[String]): Unit = {
    import File._

    def toSpace(seq: List[String]) = for (x <- 0 until seq.length; out <- seq(x) match {
      case l => for(y <- 0 until l.length) yield l(y) match {
        case 'L' => Available(x, y)
        case '#' => Occupied(x, y)
        case '.' => Floor
      }
    }) yield out

    @scala.annotation.tailrec
    def stabilize(seq: Array[Space]): Array[Space] = {
      def index(x: Int, y: Int) = y + x * 95
      def inRange(x: Int, y: Int, r: Int, c: Int) = !(y == 0 && c == -1) && !(y == 94 && c == 1)
      def neighbors(x: Int, y: Int) = for { r <- -1 to 1; out <- for { c <- -1 to 1 if inRange(x,y,r,c) } yield index(x + r, y + c) } yield out

      def in(seq: Array[Space])(x: Int, y: Int) = {
        neighbors(x, y).filter(_ >= 0).filter(_ < seq.length).map(seq(_))
      }

      def areAdjacentUnoccupied(seq: Array[Space], x: Int, y: Int) = {
        in(seq)(x, y).count(_ match {
          case Available(_, _) | Floor => false
          case Occupied(_, _) => true
        }) == 0
      }

      def areAdjacentOccupied(seq: Array[Space], x: Int, y: Int) = {
        in(seq)(x, y).count(_ match {
          case Available(_, _) | Floor => false
          case Occupied(_, _) => true
        }) >= 5
      }

      seq.map {
        case Available(x, y) if areAdjacentUnoccupied(seq, x, y) => Occupied(x, y)
        case Occupied(x, y) if areAdjacentOccupied(seq, x, y) => Available(x, y)
        case Floor => Floor
        case same@_ => same
      } match {
        case other@_ if(other.sameElements(seq)) => other
        case other@_ => stabilize(other)
      }
    }

    @scala.annotation.tailrec
    def stabilizeTrail(seq: Array[Space]): Array[Space] = {
      def index(x: Int, y: Int) = y + x * 95
      def inRange(x: Int, y: Int, r: Int, c: Int) = !(y == 0 && c == -1) && !(y == 94 && c == 1)
      def neighbors(x: Int, y: Int) = for { r <- -1 to 1; out <- for { c <- -1 to 1 if inRange(x,y,r,c) } yield index(x + r, y + c) } yield out

      def in(seq: Array[Space])(x: Int, y: Int) = {
        neighbors(x, y).filter(_ >= 0).filter(_ < seq.length).map(seq(_))
      }

      def areAdjacentUnoccupied(seq: Array[Space], x: Int, y: Int) = {
        in(seq)(x, y).count(_ match {
          case Available(_, _) | Floor => false
          case Occupied(_, _) => true
        }) == 0
      }

      def areAdjacentOccupied(seq: Array[Space], x: Int, y: Int) = {
        in(seq)(x, y).count(_ match {
          case Available(_, _) | Floor => false
          case Occupied(_, _) => true
        }) >= 5
      }

      seq.map {
        case Available(x, y) if areAdjacentUnoccupied(seq, x, y) => Occupied(x, y)
        case Occupied(x, y) if areAdjacentOccupied(seq, x, y) => Available(x, y)
        case Floor => Floor
        case same@_ => same
      } match {
        case other@_ if(other.sameElements(seq)) => other
        case other@_ => stabilizeTrail(other)
      }
    }

    println(stabilize({read("day11") match { case seq@_ => toSpace(seq) }}.toArray).count { case Occupied(_, _) => true; case _ => false } + " is the answer") //Part 1
    println(stabilizeTrail({read("day11") match { case seq@_ => toSpace(seq) }}.toArray).count { case Occupied(_, _) => true; case _ => false } + " is the answer") //Part 1
  }
}
