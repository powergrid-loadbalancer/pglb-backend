package servicebus

// Include the following imports to use Service Bus APIs
import java.io.{BufferedReader, IOException, InputStreamReader}
import java.net.{InetAddress, Socket, UnknownHostException}
import java.text.SimpleDateFormat
import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.util.{Calendar, Locale}

import bot.BotManager
import play.api.libs.json._

import scala.io.BufferedSource
import scala.runtime.RichInt

class ServiceBus(socket: Socket) {
  var head: JsArray = JsArray(List())
  var buffer: Map[Int, JsObject] = Map()
  var currentTime: Option[Calendar] = None
  var isRecieving = true
  var loop = true
  var runnable = new Thread(new Runnable {
    override def run(): Unit = loopCalc()
  })

  def loopCalc(): Unit = {
    var bufferedReader: BufferedReader = null
    try
      bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream))
    catch {
      case e: UnknownHostException => {
        e.printStackTrace()
      }
      case e: IOException => {
        e.printStackTrace()
      }
    }
    if (bufferedReader == null) {
      isRecieving = false;
    }
    try {
      while (loop) {
            val line: String = bufferedReader.readLine
            handleJson(line)
          }
    } catch {
       case _ => {
         socket.close()
         isRecieving = false
       }
    }
  }

  def handleJson(line: String): Option[JsObject] = {
    System.out.println(line)
    val maybeObject: Option[JsObject] = Json.parse(line) match {
      case jsObj: JsObject => Some(jsObj)
      case other => {
        System.err.println("line: <" + line + "> is not an jsonObject")
        None
      }
    }
    maybeObject.foreach(obj => {
      val tupleOpt: Option[(String, String)] = obj.value.get("Time")
          .flatMap(time => {
            time match {
              case string: JsString => Some(string.value)
              case other => None
            }
          })
        .flatMap(time => {
          obj.value.get("Date").flatMap(jsval => {
            jsval match {
              case string: JsString => Some(string.value)
              case other => None
            }
          }).map(date => (time, date))
        })

      val calendarOpt: Option[Calendar] = tupleOpt.map(tuple => {
        val date: String = tuple._2 + " " + tuple._1
        val cal: Calendar = Calendar.getInstance()
        val sdf: SimpleDateFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm")
        cal.setTime(sdf.parse(date))
        cal
      })

      obj.value.get("Meter").flatMap(value => {
        value match {
          case numb: JsString => Some(numb.value.toInt)
          case JsNull => Some(-1)
          case other => None
        }
      }).foreach(id => buffer = buffer.+((id, obj)))

      if (calendarOpt.isDefined) {
        if (currentTime.forall(current => calendarOpt.get.after(current))) {
          val oldTime: Option[Calendar] = currentTime
          currentTime = calendarOpt
          if (oldTime.isDefined) {
            System.out.println("new head!")
            val buyers: List[JsObject] = calculate(buffer)
            val finalData: JsArray = JsArray(buyers)
            buffer = Map()
            head = finalData
          }
        } else //if (currentTime.forall(current => calendarOpt.get.equals(current))) {
        {

        }
      }
    })
    maybeObject
  }

  def calculate(buffer: Map[Int, JsObject]): List[JsObject] = {
    buffer.get(-1) match {
      case None => System.err.println("no Producer received");Nil
      case Some(producer) => {
        val maxProd = producer.value.get("Value").get.asInstanceOf[JsString].value.replace(",", ".").toDouble
        val activeBots: List[JsObject] = buffer.filter(tuple => tuple._1 != -1)
          .filter(tuple => BotManager.bots.get(tuple._1).isDefined)
          .values.toList

        val totalConsume: Double = activeBots
          .foldRight(0.0)((tuple, akk) => {
            akk + tuple.value.get("Value").get.asInstanceOf[JsString].value.replace(",", ".").toDouble
          })
        var diff = maxProd - (totalConsume * 2300)
        if (diff <= 0) {
          buffer.toList.map(tuple => {
            val (key, value) = tuple
            val valueTemp = JsString(value.value.get("Value").get.asInstanceOf[JsString].value.replace(",", "."))
            value.+("buyingValue", valueTemp)
              .-("Value")
              .+("normalValue",valueTemp)
          })
        } else {
          buffer.toList.map(tuple => {
            val (key, value) = tuple
            if (diff <= 0 || key == -1) {
              val valueTemp = JsString(value.value.get("Value").get.asInstanceOf[JsString].value.replace(",", "."))
              value.+("buyingValue", valueTemp)
                .-("Value")
                .+("normalValue",valueTemp)
            } else {
              val buy: Double = BotManager.getBot(key).buy(diff)
              diff = diff - buy
              val newValue: Double = value.value.get("Value").get.asInstanceOf[JsString].value.replace(",", ".").toDouble + buy
              val valueTemp = JsString(value.value.get("Value").get.asInstanceOf[JsString].value.replace(",", "."))
              value.+("buyingValue", JsString(newValue.toString))
                .-("Value")
                .+("normalValue",valueTemp)
            }
          })
        }
      }
    }
  }
}

object ServiceBus {
  def get(ip: String): Either[Exception, ServiceBus] = {
    try {
      val socket: Socket = new Socket(InetAddress.getByName(ip), 11000)
      Right(new ServiceBus(socket))
    } catch {
      case e: Exception => Left(e)
    }
  }
}
