package practice

object UserInput {

  class TextInput {
    var text: Array[Char] = new Array[Char](0)
    def add(c: Char): Unit = {
      text = text :+ c
    }

    def getValue(): String = {
      var output = ""
      for (a <- text) output = output + String.valueOf(a)
      output
    }
  }

  class NumericInput extends TextInput {
    override def add(c: Char): Unit = {
      val matchString = "[0-9]".r
      val s = String.valueOf(c);
      s match {
        case matchString() => super.add(c)
        case _ =>
      }
    }
  }

  def main(args: Array[String]) = {
    // Example case
    val input: TextInput = new NumericInput()
    input.add('1')
    input.add('a')
    input.add('0')
    println(input.getValue())
  }
}