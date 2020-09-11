package ben

import scala.util.Try

sealed trait BenValue

case class BenString(value: String) extends BenValue

case class BenInteger(value: Long) extends BenValue

case class BenList(value: List[BenValue]) extends BenValue

case class BenDictionary(value: Map[BenString, BenValue]) extends BenValue

object BenValue {
  def parse(s: String): Try[BenValue] = {
    def parse(f: String => (BenValue, String)): Try[BenValue] = {
      for {
        (benValue, _) <- Try(f(s))
      } yield benValue
    }

    s.head match {
      case c if c.isDigit => parse(benString)
      case 'i' => parse(benInteger)
      case 'l' => parse(s => benList(s.tail))
      case 'd' => parse(s => benDictionary(s.tail))
    }
  }

  private def benString(s: String): (BenString, String) = {
    val (lengthOfBenString, benString) = s.splitAt(s.indexOf(':'))
    (BenString(benString.slice(1, lengthOfBenString.toInt + 1)), s.drop(lengthOfBenString.length + lengthOfBenString.toInt + 1))
  }

  private def benInteger(s: String): (BenInteger, String) = {
    val maybeNumber = s.drop(1).takeWhile(_ != 'e')
    (BenInteger(maybeNumber.toLong), s.drop(maybeNumber.length + 2))
  }

  private def benList(s: String, acc: List[BenValue] = List.empty[BenValue]): (BenList, String) = {
    s.head match {
      case c if c.isDigit =>
        val (string, remainder) = benString(s)
        benList(remainder, string :: acc)
      case 'i' =>
        val (integer, remainder) = benInteger(s)
        benList(remainder, integer :: acc)
      case 'l' =>
        val (list, remainder) = benList(s.tail)
        benList(remainder, list :: acc)
      case 'd' =>
        val (dictionary, remainder) = benDictionary(s.tail)
        benList(remainder, dictionary :: acc)
      case 'e' => (BenList(acc.reverse), s.tail)
    }
  }

  private def benDictionary(s: String, acc: Map[BenString, BenValue] = Map.empty[BenString, BenValue]): (BenDictionary, String) = {
    if (s.head == 'e') {
      (BenDictionary(acc), s.tail)
    } else {
      val (benKey, benValueRemainder) = benString(s)
      benValueRemainder.head match {
        case c if c.isDigit =>
          val (string, remainder) = benString(benValueRemainder)
          benDictionary(remainder, acc + (benKey -> string))
        case 'i' =>
          val (integer, remainder) = benInteger(benValueRemainder)
          benDictionary(remainder, acc + (benKey -> integer))
        case 'l' =>
          val (list, remainder) = benList(benValueRemainder.tail)
          benDictionary(remainder, acc + (benKey -> list))
        case 'd' =>
          val (dictionary, remainder) = benDictionary(benValueRemainder.tail)
          benDictionary(remainder, acc + (benKey -> dictionary))
      }
    }
  }
}