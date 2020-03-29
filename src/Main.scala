
import scala.annotation.tailrec
import scala.math.abs

object Main extends App {
  val days: List[String] = List("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")
  val products: Map[String, Double] = Map("Apple" -> 10.0, "Car" -> 123456.0, "Diploma" -> 21000.0)
  val listWithZeros: List[Int] = List(1, 2, 0, 3, 4, 0, 13, 0, 5, 6, 0, 0, -6, 0, -3, 0, 7, -1)
  println("1a: " + concatenateWithFor(days))
  println("1b: " + concatenateWithForAndIf(days))
  println("1c: " + concatenateWithWhile(days))
  println("2a: " + concatenateWithRecurrence(days))
  println("2b: " + concatenateWithRecurrenceReversed(days))
  println("3: " + concatenateWithTailRecurrence(days))
  println("4a: " + concatenateWithFoldl(days))
  println("4b: " + concatenateWithFoldr(days))
  println("4c: " + concatenateWithFoldlAndIf(days))
  println("5: " + discount(products, 0.1))
  println("6: " + tupleToString("String", 1, true))
  println("7a: " + getPriceOfProduct(products, "Apple"))
  println("7b: " + getPriceOfProduct(products, "Window"))
  println("8: " + removeZeros(listWithZeros))
  println("9: " + addOne(listWithZeros))
  println("10: " + filter(listWithZeros.map(element => element.toDouble)))

  def concatenateWithFor(list: List[String]): String = {
    var concatenatedString: String = list.head
    for (element <- list.tail) {
      concatenatedString = concatenatedString.concat(", " + element)
    }
    concatenatedString
  }

  def concatenateWithForAndIf(list: List[String]): String = {
    var concatenatedString: String = list.head
    for (element <- list.tail if element.startsWith("P")) {
      concatenatedString = concatenatedString.concat(", " + element)
    }
    concatenatedString
  }

  def concatenateWithWhile(list: List[String]): String = {
    var index: Int = 1
    var concatenatedString: String = list.head
    while (index < list.size) {
      concatenatedString = concatenatedString.concat(", " + list(index))
      index += 1
    }
    concatenatedString
  }

  def concatenateWithRecurrence(list: List[String]): String = {
    def iter(list: List[String]): String = {
      if (list.size == 1) list.head
      else list.head + ", " + iter(list.tail)
    }

    iter(list)
  }

  def concatenateWithRecurrenceReversed(list: List[String]): String = {
    def iter(list: List[String]): String = {
      if (list.size == 1) list.head
      else list.head + ", " + iter(list.tail)
    }

    iter(list.reverse)
  }

  def concatenateWithTailRecurrence(list: List[String]): String = {
    @tailrec
    def iter(remainingList: List[String], result: String): String = {
      val nextElement = remainingList.head
      val nextResult = result.concat(", " + nextElement)
      if (nextElement == list.last) nextResult
      else iter(remainingList.tail, nextResult)
    }

    iter(list.tail, list.head)
  }

  def concatenateWithFoldl(list: List[String]): String = list.tail.foldLeft(list.head)(_ + ", " + _)

  def concatenateWithFoldr(list: List[String]): String = list.dropRight(1).foldRight(list.last)(_ + ", " + _)

  def concatenateWithFoldlAndIf(list: List[String]): String = list.tail.foldLeft(list.head) { (result: String, next: String) => result.concat(", " + next) }

  def discount(map: Map[String, Double], percentage: Double): Map[String, Double] = map.view.mapValues(value => value - value * percentage).toMap

  def tupleToString(tuple: (String, Int, Boolean)): String = "String: " + tuple._1 + ", Int: " + tuple._2 + ", Boolean: " + tuple._3

  def getPriceOfProduct(productMap: Map[String, Double], productName: String): Option[Double] = productMap.get(productName)

  def removeZeros(list: List[Int]): List[Int] = {
    @tailrec
    def iter(remainingList: List[Int], result: List[Int]): List[Int] = {
      if (remainingList.isEmpty) result
      else if (remainingList.head != 0) {
        iter(remainingList.tail, result.appended(remainingList.head))
      } else {
        iter(remainingList.tail, result)
      }
    }

    iter(list, List())
  }

  def addOne(list: List[Int]): List[Int] = {
    list.map(element => element + 1)
  }

  def filter(list: List[Double]): List[Double] = {
    list.filter(element => element < 12)
      .filter(element => element > -5)
      .map(element => abs(element))
  }
}
