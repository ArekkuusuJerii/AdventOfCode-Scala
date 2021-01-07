package practice

import scala.annotation.unchecked.uncheckedVariance

object FlattenMap {

  case class :::[+A](private val element: A, var next: Group[A@uncheckedVariance]) extends Group[A]
  case object Non extends Group[Nothing]
  abstract class Group[+A] {
    def :::[B >: A](element: B): Group[B] = new :::(element, this)

    def tail: Option[A] = {
      @scala.annotation.tailrec
      def recursive(b: Group[A]): Option[A] = b match {
        case a ::: b => b match {
          case Non => Some(a)
          case _ => recursive(b)
        }
        case _ => None
      }

      recursive(this)
    }

    def flatten[B >: A](f: (B, A) => B): Group[B] = {
      @scala.annotation.tailrec
      def recursive(prev: B, next: Group[A]): Group[B] = {
        next match {
          case a ::: b => recursive(f(prev, a), b)
          case _ => prev ::: Non
        }
      }

      this match {
        case a ::: b => recursive(a, b)
        case Non => Non
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val group = "are" ::: "you" ::: "my" ::: "little" ::: "pogchamp" ::: "?" ::: Non
    val groupNum = 1 ::: 2 ::: 3 ::: 4 ::: 5 ::: 6 ::: Non

    val concat = (a: String, b: String) => a + " " + b
    println(group.tail match { case Some(a) => a; case None => "empty" })
    println(group.flatten(concat).tail match { case Some(a) => a; case None => "empty" })

    val sum = (a: Int, b: Int) => a + b
    println(groupNum.tail match { case Some(a) => a; case None => "empty" })
    println(groupNum.flatten(sum).tail match { case Some(a) => a; case None => "empty" })

    //???
    println((9 ::: groupNum.flatten(sum)).flatten(sum).tail match { case Some(a) => a; case None => "empty" })
  }
}