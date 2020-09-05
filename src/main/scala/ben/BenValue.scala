package ben

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

sealed trait BenValue

case class BenString(value: String) extends BenValue

case class BenInteger(value: Long) extends BenValue

case class BenList(value: List[BenValue]) extends BenValue

case class BenDictionary(value: Map[BenString, BenValue]) extends BenValue

object BenValue {
  def parse(s: String): Try[BenValue] = {
    def parse(f: String => Try[(BenValue, String)], errorMessage: => String): Try[BenValue] = {
      for {
        (benValue, remainder) <- f(s)
        result <- if (remainder.nonEmpty) Failure(new RuntimeException(errorMessage)) else Success(benValue)
      } yield result
    }

    s.head match {
      case c if c.isDigit => parse(benString, s"$s is not a string")
      case 'i' => parse(benInteger, s"$s is not an integer")
      case 'l' => parse(s => benList(s.tail), s"$s is not a list")
      case 'd' => parse(s => benDictionary(s.tail), s"$s is not a dictionary")
    }
  }

  private def benString(s: String): Try[(BenString, String)] = Try {
    if (!s.contains(':')) {
      throw new RuntimeException(s"Splitter ':' not found in $s Bencoded string's format is `<string length encoded in base ten ASCII>:<string data>`")
    }

    val (lengthOfBenString, benString) = s.splitAt(s.indexOf(':'))

    if (!lengthOfBenString.forall(_.isDigit)) {
      throw new RuntimeException(s"$lengthOfBenString is not an integer. Bencoded string's format is `<string length encoded in base ten ASCII>:<string data>`")
    }

    if (lengthOfBenString.toInt > benString.drop(1).length) {
      throw new RuntimeException(s"$lengthOfBenString is greater than bencoded string's length. Bencoded string's format is `<string length encoded in base ten ASCII>:<string data>`")
    }

    (BenString(benString.slice(1, lengthOfBenString.toInt + 1)), s.drop(lengthOfBenString.length + lengthOfBenString.toInt + 1))
  }

  private def benInteger(s: String): Try[(BenInteger, String)] = Try {
    val maybeNumber = s.drop(1).takeWhile(_ != 'e')
    if (maybeNumber.exists(!_.isDigit)) throw new RuntimeException(s"$maybeNumber is not an integer. Bencoded integer's format is `i<integer encoded in base ten ASCII>e`")
    (BenInteger(maybeNumber.toLong), s.drop(maybeNumber.length + 2))
  }

  private def benList(s: String, acc: List[BenValue] = List.empty[BenValue]): Try[(BenList, String)] = {
    def parse(f: String => Try[(BenValue, String)]): Try[(BenList, String)] = {
      f(s) match {
        case Success((benValue, remainder)) =>
          benList(remainder, benValue :: acc)
      }
    }

    if (s.isEmpty) Failure(new RuntimeException("Invalid bencoded list"))
    else {
      s.head match {
        case c if c.isDigit => parse(benString)
        case 'i' => parse(benInteger)
        case 'l' => parse(s => benList(s.tail))
        case 'd' => parse(s => benDictionary(s.tail))
        case 'e' => Try((BenList(acc.reverse), s.tail))
        case c => Failure(new RuntimeException(s"Unexpected character $c ($s)"))
      }
    }
  }

  private def benDictionary(s: String, acc: Map[BenString, BenValue] = Map.empty[BenString, BenValue]): Try[(BenDictionary, String)] = {
    def parse(benKey: BenString, remainder: String, f: String => Try[(BenValue, String)]): Try[(BenDictionary, String)] = {
      f(remainder) match {
        case Success((benValue, remainder)) =>
          benDictionary(remainder, acc + (benKey -> benValue))
      }
    }

    def parseKey(s: String): Try[(BenString, String)] = {
      s.head match {
        case 'e' => Success(BenString(""), s)
        case _ => benString(s)
      }
    }

    if (s.isEmpty) Failure(new RuntimeException("Invalid bencoded dictionary"))
    else {
      for {
        (benKey, remainder) <- parseKey(s)
        benValue <- remainder.head match {
          case c if c.isDigit => parse(benKey, remainder, benString)
          case 'i' => parse(benKey, remainder, benInteger)
          case 'l' => parse(benKey, remainder, s => benList(s.tail))
          case 'd' => parse(benKey, remainder, s => benDictionary(s.tail))
          case 'e' => Try((BenDictionary(acc), s.tail))
          case c => Failure(new RuntimeException(s"Unexpected character $c ($s)"))
        }
      } yield benValue
    }
  }
}