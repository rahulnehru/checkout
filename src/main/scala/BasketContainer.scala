case class BasketContainer(var basket: Map[StoreKeepingUnit, Int]) {


  def calculateBasketTotal(): Double = {
    basket.foldLeft(0.0)(
      (cumulativePrice, nextItem) =>
        nextItem match {
          case (StoreKeepingUnit(name, unitPrice, Some(x)), quantity) if unitPrice >= 0 && quantity >=0 => ((quantity / x.offerQuantity).toDouble * x.offerPrice) + ((quantity % x.offerQuantity) * unitPrice) + cumulativePrice
          case (StoreKeepingUnit(name, unitPrice, None), quantity) if unitPrice >= 0 && quantity >=0 => (unitPrice * quantity) + cumulativePrice
          case _ => throw new IllegalStateException(s"Could not calculate the basket price for ${nextItem._1.asInstanceOf[StoreKeepingUnit].name}")
        }
    )
  }

  def addToBasket(sku: StoreKeepingUnit): Unit =
    if(basket.contains(sku)) basket += (sku -> (basket(sku) + 1))
    else basket += (sku -> 1)


}
