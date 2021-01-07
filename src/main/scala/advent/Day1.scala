package advent

import scala.util.Sorting

object Day1 {

  def partOne(): Unit = {
    import File._

    val date = 2020 // Must find!

    val numbers = read("day1").iterator.map(_.toInt).toArray;
    Sorting.quickSort(numbers)
    var l = 0
    var r = numbers.length - 1
    while (l < r) {
      if ((numbers(l) + numbers(r)) == date) {
        println(numbers(l) * numbers(r))
        l = r
      } else if ((numbers(l) + numbers(r)) < date) {
        l = l + 1
      } else {
        r = r - 1
      }
    }
  }

  def partTwo(): Unit = {
    import File._

    val date = 2020 // Must find!

    val numbers = read("day1").iterator.map(_.toInt).toArray;
    Sorting.quickSort(numbers)

    for (number <- numbers) {
      var l = 0
      var r = numbers.length - 1
      while (l < r) {
        if ((numbers(l) + numbers(r) + number) == date) {
          println(numbers(l) * numbers(r) * number)
          return
        } else if ((numbers(l) + numbers(r) + number) < date) {
          l += 1
        } else {
          r -= 1
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    partOne()
    partTwo()
  }
}
