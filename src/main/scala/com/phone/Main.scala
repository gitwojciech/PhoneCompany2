package com.phone

object Main extends App {


   val dailyCharges = PhoneCompanyLog.getDailyCharges("src/main/resources/calls.log")

  dailyCharges.foreach(e => println(s"User ${e._1}: today charges are  ${(e._2.toDouble)/100}"))


}
