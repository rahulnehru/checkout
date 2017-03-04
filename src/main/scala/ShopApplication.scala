
import scala.util.Try

object ShopApplication {
  import Store.itemsToSell

  val interface = new ConsoleInterface()

  val welcome = "Welcome to the shop \n \n"
  val itemAdded = "Item added to basket, your total so far is {0}"
  val doNotStock = "Sorry, we do not stock that item!"
  val priceSku = "The price of {0} today is {1} pence each, and {2}"
  val checkoutItem = "Ok, now lets start shopping for your items, please type in the names of items you'd like to buy followed by enter. " +
    "\n When you are done, please press enter and your total will be calculated for you... "
  val noOffer = "there is no offer on this today."
  val total = "Thank you, your total is {0} pence"
  val correctPrice = "If the unit price is incorrect, please type the correct amount in pence, and press enter, or press enter to continue"
  val couldNotParse = "Could not parse input in pence"
  val updatedPrice = "The unit price of {0} has been updated to {1}"
  val inputLessThanZero = "The price cannot be less than 0 pence, continuing with original value"


  /*
      Interrogates the interface to collect items which the customer wishes to purchase.
   */

  def collectItems(): BasketContainer = {
    interface.printMsg(checkoutItem)
    val basketContainer = BasketContainer(Map.empty[StoreKeepingUnit, Int])
    Iterator.continually(interface.readInput).takeWhile(_.nonEmpty) foreach { itemName => lookUpAndAddItem (itemName, basketContainer)}
    basketContainer
  }

  /*
    Ensures the items which the customer wishes to purchase are available
   */

  def lookUpAndAddItem(itemName: String, basketContainer: BasketContainer) = {
    if(itemsToSell.exists(_.name==itemName.trim)) {
      basketContainer.addToBasket(itemsToSell.find(_.name==itemName).get)
      interface.printMsg(itemAdded, basketContainer.calculateBasketTotal())
    }
    else
      interface.printMsg(doNotStock)
  }

  /*
    Interrogates the interface to prompt user to ensure all items prices are correct
   */

  def checkPrices() = {
    itemsToSell foreach { sku =>{
      def specialPriceText: String = if(sku.specialPrice.isDefined) s"there you can buy ${sku.specialPrice.get.offerQuantity} for ${sku.specialPrice.get.offerPrice} pence." else noOffer
      interface.printMsg(priceSku, sku.name, sku.unitPrice, specialPriceText)
      interface.printMsg(correctPrice)
      val input  = interface.readInput
      validateNewUnitPrice(sku, input)
      }
    }
  }

  /*
    Validates input is
   */

  def validateNewUnitPrice(sku: StoreKeepingUnit, input: String) = {
    if (input.isEmpty) -1
    else Try(input.toInt).toEither match {
      case Right(i) if i >=0  => itemsToSell.update(itemsToSell.indexWhere(_.name == sku.name), StoreKeepingUnit(sku.name, i, sku.specialPrice)); interface.printMsg(updatedPrice, sku.name, i)
      case Right(i)  => interface.printMsg(inputLessThanZero); -1
      case _ => throw new NumberFormatException(s"Could not parse $input as a valid price in pence")
    }
  }

  def main(args: Array[String]) {
    interface.printMsg(welcome)
    checkPrices()
    println()
    val basket = collectItems()
    interface.printMsg(total, basket.calculateBasketTotal())
  }
}
