package practice

object ListInverse {

  sealed abstract class LinkedList[+A] {
    def :::[B >: A](next: B) = Yes(next, this)
    def reverse = {
      def recursive(prev: LinkedList[A], next: LinkedList[A]): LinkedList[A] = next match {
        case Yes(v, No) => Yes(v, prev)
        case Yes(v, n) => recursive(v ::: prev, n)
      }
      this match {
        case No | Yes(_, No) => this
        case Yes(v, n) => recursive(v ::: No, n)
      }
    }
  }
  case class Yes[B](value: B, next: LinkedList[B]) extends LinkedList[B]
  case object No extends LinkedList[Nothing]

  sealed abstract class ManyMap[+A]
  case class Leaf[B](value: B, branch: LinkedList[ManyMap[B]] = No) extends ManyMap[B]

  def main(args: Array[String]): Unit = {
    val head = "f" ::: "e" ::: "d" ::: "c" ::: "b" ::: "a" ::: No
    println(head)
    println(head.reverse)
    val top = Leaf("1", Leaf("1a") ::: Leaf("1b") ::: No)
    println(top)
  }
}
