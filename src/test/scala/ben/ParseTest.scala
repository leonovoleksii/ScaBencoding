package ben

import org.specs2.mutable.Specification

class ParseTest extends Specification {
  "BenValue.parse" should {

    "decode bencoded string" in {
      BenValue.parse("10:somestring") must beSuccessfulTry(BenString("somestring"))

      BenValue.parse("14:somestring") must beSuccessfulTry(BenString("somestring"))

      BenValue.parse("5:somestring") must beSuccessfulTry(BenString("somes"))

      BenString.parse("5:hello") must beSuccessfulTry(BenString("hello"))

      BenValue.parse("10somestring") must beFailedTry

      BenValue.parse("1notanumber:somestring") must beFailedTry

      BenString.parse("hello") must beFailedTry
    }

    "decode bencoded integer" in {
      BenValue.parse("i314e") must beSuccessfulTry(BenInteger(314))

      BenValue.parse("i314easd") must beSuccessfulTry(BenInteger(314))

      BenInteger.parse("i314e") must beSuccessfulTry(BenInteger(314))

      BenValue.parse("i3wrongsymbolse") must beFailedTry

      BenValue.parse("i314") must beFailedTry

      BenInteger.parse("314e") must beFailedTry
    }

    "decode bencoded list" in {
      BenValue.parse("l10:somestringi314ee") must beSuccessfulTry(BenList(List(BenString("somestring"), BenInteger(314L))))

      BenValue.parse("l10:somestringli314eee") must beSuccessfulTry(BenList(List(BenString("somestring"), BenList(List(BenInteger(314L))))))

      BenList.parse("l10:somestringli314eee") must beSuccessfulTry(BenList(List(BenString("somestring"), BenList(List(BenInteger(314L))))))

      BenValue.parse("l10:somestringli314ee") must beFailedTry

      BenList.parse("10:somestringli314ee") must beFailedTry
    }

    "decode bencoded dictionary" in {
      BenValue.parse("d7:somekeyi314e12:someotherkeyli341eee") must beSuccessfulTry(
        BenDictionary(Map(BenString("somekey") -> BenInteger(314L), BenString("someotherkey") -> BenList(List(BenInteger(341L))))))

      BenValue.parse("d7:somekeyi314e12:someotherkeyd10:anotherkeyi341eee") must beSuccessfulTry(
        BenDictionary(Map(BenString("somekey") -> BenInteger(314L), BenString("someotherkey") -> BenDictionary(Map(BenString("anotherkey") -> BenInteger(341L))))))

      BenDictionary.parse("d7:somekeyi314e12:someotherkeyli341eee") must beSuccessfulTry(
        BenDictionary(Map(BenString("somekey") -> BenInteger(314L), BenString("someotherkey") -> BenList(List(BenInteger(341L))))))

      BenValue.parse("d7:somekeyi314e12:someotherkeyd10:anotherkeyi341ee") must beFailedTry

      BenDictionary.parse("7:somekeyi314e12:someotherkeyli341eee") must beFailedTry
    }
  }
}
