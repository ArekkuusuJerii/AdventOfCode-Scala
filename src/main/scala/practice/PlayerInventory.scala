package practice

class PlayerInventory {
  private var items: Vector[String] = Vector("lumber", "stone", "magic potion")

  def getItems(): Vector[String] = {
    items
  }

  def addToInventory(item: String): Unit = {
    items = items :+ item
  }

  def dropFromInventory(item: String): Unit = {
    val itemsCopy = Vector().appendedAll(items)
    val i = itemsCopy.indexOf(item)
    items = itemsCopy.dropRight((itemsCopy.length) - i) :++ itemsCopy.drop(i + 1)
  }
}

object PlayerInventory {
  def main(args: Array[String]) = {
    var p: PlayerInventory = new PlayerInventory

    p.addToInventory("lumber")
    p.dropFromInventory("stone")

    println(p.getItems())
  }
}