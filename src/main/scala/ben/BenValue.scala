package ben

import scala.util.Try

sealed trait BenValue

case class BenString(value: String) extends BenValue

case class BenInteger(value: Long) extends BenValue

case class BenList(value: List[BenValue]) extends BenValue

case class BenDictionary(value: Map[BenString, BenValue]) extends BenValue

object BenString {
  def parse(s: String): Try[BenString] = Try {
    if (s.head.isDigit) {
      BenValue.benString(s)._1
    } else {
      throw new RuntimeException(s"Expected bencoded string (<string length>:<string data>): $s")
    }
  }
}

object BenInteger {
  def parse(s: String): Try[BenInteger] = Try {
    if (s.startsWith("i")) {
      BenValue.benInteger(s)._1
    } else {
      throw new RuntimeException(s"Expected bencoded integer (i<integer>e): $s")
    }
  }
}

object BenList {
  def parse(s: String): Try[BenList] = Try {
    if (s.startsWith("l")) {
      BenValue.benList(s.tail)._1
    } else {
      throw new RuntimeException(s"Expected bencoded list (l<bencoded values>e): $s")
    }
  }
}

object BenDictionary {
  def parse(s: String): Try[BenDictionary] = Try {
    if (s.startsWith("d")) {
      BenValue.benDictionary(s.tail)._1
    } else {
      throw new RuntimeException(s"Expected bencoded dictionary (d<bencoded string><bencoded value>e): $s")
    }
  }
}

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

  private[ben] def benString(s: String): (BenString, String) = {
    val (lengthOfBenString, benString) = s.splitAt(s.indexOf(':'))
    (BenString(benString.slice(1, lengthOfBenString.toInt + 1)), s.drop(lengthOfBenString.length + lengthOfBenString.toInt + 1))
  }

  private[ben] def benInteger(s: String): (BenInteger, String) = {
    val end = s.indexOf('e')
    val maybeNumber = s.slice(1, end)
    (BenInteger(maybeNumber.toLong), s.drop(maybeNumber.length + 2))
  }

  private[ben] def benList(s: String, acc: List[BenValue] = List.empty[BenValue]): (BenList, String) = {
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

  private[ben] def benDictionary(s: String, acc: Map[BenString, BenValue] = Map.empty[BenString, BenValue]): (BenDictionary, String) = {
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