package bot

/**
  */
object BotManager {
  var bots: Map[Int, BuyBot] = Map()
  var disabled: Map[Int, BuyBot] = Map()
  var capacity: Double = 0

  def setCapacity(newCapacity: Double): Unit = {
    this.synchronized {
      capacity = newCapacity
      bots.map(tuple => BuyBot(capacity, tuple._2.depleted))
    }
  }

  def getBot(id: Int): BuyBot = {
    bots.get(id).orElse(disabled.get(id)).getOrElse[BuyBot]({
      val bot: BuyBot = new BuyBot(capacity, 0)
      bots = bots.+((id, bot))
      bot
    })
  }

  def changeBehaviour(id: Int, newMax: Int): Unit = {
    this.synchronized {
      bots = bots.+((id, new BuyBot(newMax, bots.get(id).map(_.depleted).getOrElse(0))))
    }
  }

  def disable(id: Int, newMax: Int): Unit = {
    this.synchronized {
      val buyBot: BuyBot = bots.get(id).orElse(disabled.get(id)).getOrElse(new BuyBot(0))
      disabled = disabled.+((id, buyBot))
      bots = bots - id
    }
  }

  def enable(id: Int): Unit = {
    this.synchronized {
      val buyBot: BuyBot = bots.get(id).orElse(disabled.get(id)).getOrElse(new BuyBot(0))
      bots = bots.+((id, buyBot))
      disabled = disabled - id
    }
  }

  def tick(): Unit = {
    bots.foreach(tuple => tuple._2.tick())
  }
}
