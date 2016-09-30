package controllers

import java.util.concurrent.atomic.AtomicBoolean

import bot.BotManager
import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsArray, JsNumber, JsValue}
import servicebus.ServiceBus

class Application extends Controller {
  var connected: AtomicBoolean = new AtomicBoolean(true)
  private var bus: Option[ServiceBus] = ServiceBus.get("51.4.231.251") match {
    case Left(e) => e.printStackTrace(); None
    case Right(bus) => bus.runnable.start();Some(bus)
  }

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def recieve = Action {
    def handleNoBus: () => JsArray = () => {
      this.synchronized {
        bus = ServiceBus.get("51.4.231.251") match {
          case Left(e) => e.printStackTrace(); None
          case Right(bus) => bus.runnable.start();Some(bus)
        }
      }
      JsArray(List())
    }
    val head: JsArray = if (bus.isDefined) {
      if (!bus.get.isRecieving) {
        bus.get.loop = false
        handleNoBus()
      } else {
        bus.get.head
      }
    } else {
      handleNoBus()
    }
    Ok(head)
  }

  def getCapacity = Action {
    Ok(BotManager.capacity)
  }

  def setCapacity() = Action { request =>
    val body: AnyContent = request.body
    val bodyIntOpt: Option[Double] = body.asJson
      .flatMap(json => {
        json match {
          case nmbr: JsNumber => Some(nmbr.value.toDouble)
          case other => None
        }
      })
    if (bodyIntOpt.isDefined) {
      BotManager.setCapacity(bodyIntOpt.get)
      Ok
    } else {
      BadRequest
    }
  }
}