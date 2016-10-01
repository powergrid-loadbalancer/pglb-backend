package bot

/**
  */
case class BuyBot(maxAmount: Double, var depleted: Double, var totalBought: Double) {
  private var recentlyBought = false

  def buy(max: Double): Double = {
    val bought: Double = Math.min(max, (BotManager.capacity)*((Math.random() + Math.random())/2).toInt)
    //val bought: Double = Math.min(max, (BotManager.capacity-depleted)*((Math.random() + Math.random())/2).toInt)
    totalBought += bought + totalBought
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
