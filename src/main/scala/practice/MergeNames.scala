package practice

object MergeNames {
  def uniqueNames(names1: Vector[String], names2: Vector[String]): Vector[String] = {
    var vector = Vector[String]()
    for (name1 <- names1) {
      if(!(vector contains name1)) {
        vector  = vector appended name1
      }
    }
    for (name2 <- names2) {
      if(!(vector contains name2)) {
        vector  = vector appended name2
      }
    }

    vector
  }

  def main(args: Array[String]): Unit = {
    val names1 = Vector("Ava", "Emma", "Olivia")
    val names2 = Vector("Olivia", "Sophia", "Emma")
    println(uniqueNames(names1, names2).mkString(","))
  }
}