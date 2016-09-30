package bot

/**
  */
case class BuyBot(maxAmount: Double, var consumed: Double) {
  private var recentlyBought = false

  def buy(max: Double): Double = {
    val bought: Double = Math.min(max, (BotManager.capacity-consumed)*((Math.random() + Math.random())/2).toInt)
    consumed = bought.toInt + consumed
    recentlyBought = true
    bought
  }

  def tick(): Unit = {
    if (recentlyBought) {
      recentlyBought = false
    } else {
      consumed = Math.min(0, consumed - 0.1)
    }
  }
}
