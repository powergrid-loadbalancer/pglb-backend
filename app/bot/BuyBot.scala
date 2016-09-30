package bot

/**
  */
case class BuyBot(maxAmount: Int) {
  def buy(max: Double): Double = {
    Math.min(max, maxAmount*((Math.random() + Math.random())/2).toInt)
  }
}
