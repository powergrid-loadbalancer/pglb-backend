package bot

/**
  */
case class BuyBot(maxAmount: Double, var depleted: Double) {
  private var recentlyBought = false

  def buy(max: Double): Double = {
    val bought: Double = Math.min(max, (BotManager.capacity-depleted)*((Math.random() + Math.random())/2).toInt)
    depleted = bought.toInt + depleted
    recentlyBought = true
    bought
  }

  def tick(): Unit = {
    if (recentlyBought) {
      recentlyBought = false
    } else {
      depleted = Math.min(0, depleted - 0.1)
    }
  }
}
