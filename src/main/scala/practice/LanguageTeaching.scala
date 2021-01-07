package practice

import scala.collection.Iterable

object LanguageTeaching {

  class LanguageStudent {
    var languages: Vector[String] = Vector()

    def addLanguage(language: String) = {
      languages = languages :+ language
    }

    def getLanguages(): Iterable[String] = {
      languages.toIterable
    }
  }

  class LanguageTeacher extends LanguageStudent {
    def teach(student: LanguageStudent, language: String): Boolean = {
      student match {
        case teacher: LanguageTeacher => false
        case _ => if (languages contains language) {
          student addLanguage language
          true
        } else {
          false
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    // Example case
    val teacher = new LanguageTeaching.LanguageTeacher
    teacher.addLanguage("English")

    val student = new LanguageTeaching.LanguageStudent
    print(teacher.teach(student, "English"))

    for (language <- student.getLanguages())
      System.out.println(language);

  }

}