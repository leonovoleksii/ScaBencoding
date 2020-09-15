package io.github.leonovoleksii.ben

import org.specs2.mutable.Specification

class EncodeTest extends Specification {
  "BenString" should {
    "be encodable" in {
      BenString("somestring").encoded must beEqualTo("10:somestring")
      BenString("anotherstring").encoded must beEqualTo("13:anotherstring")
    }
  }

  "BenInteger" should {
    "be encodable" in {
      BenInteger(314).encoded must beEqualTo("i314e")
      BenInteger(1024).encoded must beEqualTo("i1024e")
      BenInteger(-314).encoded must beEqualTo("i-314e")
    }
  }

  "BenList" should {
    "be encodable" in {
      BenList(
        List(BenList(List(BenInteger(314))), BenString("somestring"), BenDictionary(Map()))
      ).encoded must beEqualTo("lli314ee10:somestringdee")
    }
  }

  "BenDictionary" should {
    "be encodable" in {
      BenDictionary(
        Map(
          BenString("key1") -> BenInteger(314),
          BenString("key2") -> BenDictionary(Map()),
          BenString("key3") -> BenList(List(BenString("somestring")))
        )
      ).encoded must beEqualTo("d4:key1i314e4:key2de4:key3l10:somestringee")
    }
  }
}
