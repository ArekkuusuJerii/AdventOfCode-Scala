package practice

object Prefix {

  def allPrefixes(prefixLength: Int, words: LazyList[String]): LazyList[String] = {
    var set: Set[String] = Set()

    def deferProcess(s: String): LazyList[String] =
      if (s.length < prefixLength || set.contains(s)) LazyList.empty
      else {
        set = set + s
        s #:: deferProcess(s.dropRight(1))
      }

    def deferScan(t: LazyList[String]): LazyList[String] =
      if (t.isEmpty) LazyList.empty
      else deferProcess(t.head.dropRight(1)) #::: deferScan(t.tail)

    deferScan(words)
  }

  def main(args: Array[String]): Unit = {
    val words = "flow" #:: "flowers" #:: "flew" #:: "flag" #:: "fm" #:: LazyList.empty
    val p = Prefix.allPrefixes(3, words)
    p.foreach(println)
  }
}