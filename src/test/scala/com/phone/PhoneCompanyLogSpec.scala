package com.phone

package example

import org.scalatest._

class PhoneCompanyLogSpec extends FlatSpec with Matchers {

  val sampleCorrectLog:Seq[String] =
    """A 555-333-212 00:02:03
      |A 555-433-242 00:06:41
      |A 555-433-242 00:01:03
      |B 555-333-212 00:01:20
      |A 555-333-212 00:01:10
      |A 555-663-111 00:02:09
      |A 555-333-212 00:04:28
      |B 555-334-789 00:00:03
      |A 555-663-111 00:02:03
      |B 555-334-789 00:00:53
      |B 555-971-219 00:09:51
      |B 555-333-212 00:02:03
      |B 555-333-212 00:04:31
      |B 555-334-789 00:01:59
    """.stripMargin.lines.toSeq

  val sampleEmptyLog:Seq[String] =
    """
    """.stripMargin.lines.toSeq

  val sampleCorruptedLogEmptyLines:Seq[String] =
    """A 555-333-212 00:02:03
      |
      |A 555-433-242 00:06:41
      |A 555-433-242 00:01:03
      |B 555-333-212 00:01:20
      |A 555-333-212 00:01:10
      |A 555-663-111 00:02:09
      |A 555-333-212 00:04:28
      |B 555-334-789 00:00:03
      |A 555-663-111 00:02:03
      |
      |
      |
      |B 555-334-789 00:00:53
      |B 555-971-219 00:09:51
      |B 555-333-212 00:02:03
      |B 555-333-212 00:04:31
      |B 555-334-789 00:01:59
    """.stripMargin.lines.toSeq


  val sampleCorruptedLogLineFormatIncorrect:Seq[String] =
    """A 555-333-21X 00:02:03
      |A 555-433-242 00:06:4X
      |A 555-433-242 00:1:03
      |B 555-333-212 00:01:20
      |A 555-333-212 00:01:10
      |A 555-663-111 00:02:09
      |A 555-333-212 00:04:28
      |B 555-334-789 00:00:03
      |A 555-663-111 00:02:03
      |B 555-334-789 00:00:53
      |B 555-971-219 00:09:51
      |B 555-333-212 00:02:03
      |B 555-333-212 00:04:31
      |B 555-334-789 00:01:59
    """.stripMargin.lines.toSeq


  "The PhoneCompanyLog object" should "parse phone company log" in {
    PhoneCompanyLog.parse(sampleCorrectLog) shouldBe Map("A" -> 3829, "B" -> 3518)
  }

  "The PhoneCompanyLog object" should "parse empty phone company log" in {
    PhoneCompanyLog.parse(sampleEmptyLog) shouldBe Map()
  }

  "The PhoneCompanyLog object" should "parse phone company log with extra empty line" in {
    PhoneCompanyLog.parse(sampleCorruptedLogEmptyLines) shouldBe Map("A" -> 3829, "B" -> 3518)
  }


  "The PhoneCompanyLog object" should "parse phone company log with extra 3 first line incorrect format.skip[ those lines " in {
    PhoneCompanyLog.parse(sampleCorruptedLogLineFormatIncorrect) shouldBe Map("A" -> 1260, "B" -> 3518)
  }


  "Single phone call for duration under 3 minutes " should "be charged 5 units  per second" in {
    PhoneCallPrice.singleCallPrice(PhoneCall("A", "555-333-212", 123)) shouldEqual 615 //123 * 5
  }


  "Single phone call for duration over 3 minutes " should "be charged additionally 3 units  per second over 3 minutes" in {
    PhoneCallPrice.singleCallPrice(PhoneCall("A", "555-333-212", 401)) shouldEqual 2668 // 401 * 5 + 221 * 3
  }

  "Single phone call for duration of 0 sec " should "be charged 0" in {
    PhoneCallPrice.singleCallPrice(PhoneCall("A", "555-333-212", 0)) shouldEqual 0
  }

  "Single phone call for duration of 4 hours " should "be charged 114660" in {
    PhoneCallPrice.singleCallPrice(PhoneCall("A", "555-333-212", 14400)) shouldEqual 114660
  }

  "Single phone call for duration of 24 hours " should "be charged 518220" in {
    PhoneCallPrice.singleCallPrice(PhoneCall("A", "555-333-212", 86400)) shouldEqual 690660
  }


  "Single log line format " should "be in predefined format" in {
    PhoneCall.validateLogLine("""A 555-433-242 00:06:41""") shouldEqual true
  }

  "Single log line  " should "can  contain extra white chars between columns " in {
    PhoneCall.validateLogLine("""A   555-433-242   00:06:41""") shouldEqual true
  }

  "Single log line phone number" should "must contain only numbers" in {
    PhoneCall.validateLogLine("""A 555-43W-242 00:06:41""") shouldEqual false
  }

  "Single log line time" should "must contains numbers only" in {
    PhoneCall.validateLogLine("""A 555-43W-242 00:06:X1""") shouldEqual false
  }

  "Single log line time" should "must be in hh:mm:ss " in {
    PhoneCall.validateLogLine("""A 555-43W-242 00:06:1""") shouldEqual false
  }

}


