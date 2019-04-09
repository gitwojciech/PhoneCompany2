package com.phone

import java.util.logging.{Level, Logger}

import scala.util.control.NonFatal

case class PhoneCall(customerId: String, phoneNumber: String, callDurationSec: Int)

object PhoneCall {

  def parseCallDurationToSec(callDuration: String): Int = {
    callDuration.split(":").map(_.toInt).reduceLeft((x, y) => x * 60 + y)
  }

  def validateLogLine(logline: String): Boolean = {
    val logLineFormat = """\w\s+[0-9]{3}\-[0-9]{3}\-[0-9]{3}\s+[0-9]{2}\:[0-9]{2}\:[0-9]{2}$""".r
    logline match {
      case logLineFormat() => true
      case _ => false
    }
  }

  def unapply(logLine: String): Option[PhoneCall] = {
    if (logLine.trim.isEmpty) None
    else if (!validateLogLine(logLine)) {
      Logger.getLogger(getClass.getSimpleName).log(Level.WARNING, s"Failure parsing $logLine")
      None
    }
    else {
      val parts = logLine.trim.split("\\s+")
      try {

        Some(PhoneCall(parts(0), parts(1), parseCallDurationToSec(parts(2))))
      } catch {
        case NonFatal(e) =>
          Logger.getLogger(getClass.getSimpleName).log(Level.WARNING, s"Failure parsing $logLine -> ${e.getMessage}")
          None
      }
    }
  }
}

object PhoneCallPrice {

  private val flatRateLimit = 180 // 180 sec that is 3 minutes
  private val basicRate = 5 // 5 basic currency units
  private val additionalRate = 3 //3 basic currency units

  def singleCallPrice(call: PhoneCall): Int = {
    val flatRatePrice = basicRate * call.callDurationSec

    if (call.callDurationSec <= flatRateLimit) flatRatePrice
    else
      flatRatePrice + additionalRate * (call.callDurationSec - flatRateLimit)
  }
}


object PhoneCompanyLog {

  def read(fileName: String): Seq[String] = {
    val source = scala.io.Source.fromFile(fileName, "utf-8")
    val lines = source.getLines().toSeq
    //source.close()
    lines
  }

  def getDailyCharges(logFileName: String): Map[String, Int] = parse(read(logFileName))


  def parse(lines: Seq[String]): Map[String, Int] = {
    lines //Seq[String]
      .collect { case PhoneCall(phoneCall) => phoneCall }
      .groupBy(_.customerId) //group by customerId gives us Map[String,Seq[PhoneCall]]
      .mapValues(
      _.groupBy(_.phoneNumber) //Map[String,Map[String,Seq[PhoneCall]]
        .mapValues(_.map(PhoneCallPrice.singleCallPrice) //Map[String,Map[String,Seq[callPrice]]
        .sum) //Map[String,Map[String,Int] : customerId -> phoneNumber -> charge
        .values.toSeq //Map[String,Seq[Int]] : customerId -> list of charges
        .sorted
        .dropRight(1) //cutoff the greatest charge
        .sum //Map[String,Int] : customerId -> charge
    )
  }

}
