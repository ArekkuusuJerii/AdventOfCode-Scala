package advent

object Day8 {

  def main(args: Array[String]): Unit = {
    import File._

    def execute1(operations: Seq[(String, Int)]): Int = {
      @scala.annotation.tailrec
      def recursive(pointer: Int, result: Int)(visits: Array[Int]): Int = {
        if(visits(pointer) > 0) result
        else operations(pointer) match {
          case ("acc", amt) => recursive(pointer + 1, result + amt) {visits.updated(pointer, visits(pointer) + 1)}
          case ("jmp", amt) => recursive(pointer + amt, result) {visits.updated(pointer, visits(pointer) + 1)}
          case ("nop", _) => recursive(pointer + 1, result) {visits.updated(pointer, visits(pointer) + 1)}
        }
      }

      recursive(0, 0) {Array.ofDim[Int](operations.length)}
    }

    def execute2(operations: Seq[(String, Int)]): Int = {
      @scala.annotation.tailrec
      def recursive(pointer: Int, result: Int)(visits: Array[Int] = Array.ofDim[Int](operations.length))(check: (Int, Int) = (-1,0)): Int = {
        if(pointer == visits.length)
          result
        else if(pointer >= visits.length || visits(pointer) > 0)
          recursive(0, 0)(){(check._1 + 1, 0)}
        else operations(pointer) match {
          case ("acc", amt) => amt match {
            case sum => recursive(pointer + 1, result + sum)(visits.updated(pointer, visits(pointer) + 1)){check}
          }
          case ("jmp", amt) => {if(check._2 == check._1) 1 else amt} match {
            case jump => recursive(pointer + jump, result)(visits.updated(pointer, visits(pointer) + 1)){(check._1, check._2 + 1)}
          }
          case ("nop", amt) => {if(check._2 == check._1) amt.max(1) else 1} match {
            case nop => recursive(pointer + nop, result)(visits.updated(pointer, visits(pointer) + 1)){(check._1, check._2 + 1)}
          }
        }
      }

      recursive(0, 0)()()
    }

    println(execute1({ read("day8").iterator.map(_.split(' ') match { case Array(a,b) => (a, b.toInt)}) }.toList) + " is the answer") //Part 1
    println(execute2({ read("day8").iterator.map(_.split(' ') match { case Array(a,b) => (a, b.toInt)}) }.toList) + " is the answer") //Part 2
  }
}
