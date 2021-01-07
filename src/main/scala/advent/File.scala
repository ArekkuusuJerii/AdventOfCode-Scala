package advent

object File {

  def read(name: String): List[String] = {
    import scala.io.Source
    val source = Source.fromFile(s"D:\\Idea\\untitled\\src\\main\\files\\${name}.txt")
    val l = source.getLines().toList
    source.close()
    l
  }
}
