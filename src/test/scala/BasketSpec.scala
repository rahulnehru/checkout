import org.scalatest.{Matchers, WordSpec}

class BasketSpec extends WordSpec with Matchers {

  "calculateBasketTotal" should {
    "calculate the sum of the contents of the basket" when {
      "there is one item in the basket" in {
          val basket: Map[StoreKeepingUnit, Int] = Map(StoreKeepingUnit("A", 50, None) -> 1)
          new BasketContainer(basket).calculateBasketTotal() shouldBe 50
        }

      "there are multiples of the same item in the basket" in {
        val basket: Map[StoreKeepingUnit, Int] = Map(StoreKeepingUnit("A", 50, None) -> 10)
        new BasketContainer(basket).calculateBasketTotal() shouldBe 500
      }

      "there are multiple individual items in the basket" in {
        val basket: Map[StoreKeepingUnit, Int] = Map(
          StoreKeepingUnit("A", 50, None) -> 1,
          StoreKeepingUnit("B", 10, None) -> 1
        )
        new BasketContainer(basket).calculateBasketTotal() shouldBe 60
      }
    }

    "apply discounts" when {
      "there is a discounted item individual item in the basket which matches the same quantity needed for discount" in {
        val basket: Map[StoreKeepingUnit, Int] = Map(
          StoreKeepingUnit("A", 50, Some(SpecialPrice(2, 20))) -> 2
        )
        new BasketContainer(basket).calculateBasketTotal() shouldBe 20
      }

      "there is a discounted item individual item in the basket which is a multiple of the quantity needed for discount" in {
        val basket: Map[StoreKeepingUnit, Int] = Map(
          StoreKeepingUnit("A", 50, Some(SpecialPrice(2, 20))) -> 4
        )
        new BasketContainer(basket).calculateBasketTotal() shouldBe 40
      }

      "there is a quantity of an individual item in the basket which does not qualify for discount" in {
          val basket: Map[StoreKeepingUnit, Int] = Map(
            StoreKeepingUnit("A", 50, Some(SpecialPrice(2, 20))) -> 1
          )
        new BasketContainer(basket).calculateBasketTotal() shouldBe 50
      }

      "there is a quantity of an individual item with some items qualifying for discount" in {
        val basket: Map[StoreKeepingUnit, Int] = Map(
          StoreKeepingUnit("A", 50, Some(SpecialPrice(2, 20))) -> 3
        )
        new BasketContainer(basket).calculateBasketTotal() shouldBe 70
      }
    }

    "calculate total cost of basket" when {

      "basket is empty" in {
        val basket: Map[StoreKeepingUnit, Int] = Map()
        new BasketContainer(basket).calculateBasketTotal() shouldBe 0
      }

      "basket has mutiple product of which some have discounts" in {
        val basket: Map[StoreKeepingUnit, Int] = Map(
          StoreKeepingUnit("A", 10, None) -> 2,
          StoreKeepingUnit("B", 17, Some(SpecialPrice(2,20))) -> 3,
          StoreKeepingUnit("C", 15, Some(SpecialPrice(3,32))) -> 5,
          StoreKeepingUnit("D", 9, None) -> 4,
          StoreKeepingUnit("E", 16, Some(SpecialPrice(6,99))) -> 7
        )
        new BasketContainer(basket).calculateBasketTotal() shouldBe 270
      }
    }

    "throw error" when {
      "basket has an item with negative quantity" in {
        val basketContainer = new BasketContainer(Map(StoreKeepingUnit("A", 10, None) -> -1))
        an[IllegalStateException] should be thrownBy basketContainer.calculateBasketTotal()
        val exception = the [IllegalStateException] thrownBy basketContainer.calculateBasketTotal()
        exception.getMessage shouldBe "Could not calculate the basket price for A"
      }

      "basket has a negative price for an item" in {
        val basketContainer = new BasketContainer(Map(StoreKeepingUnit("A", -10, None) -> 1))
        an[IllegalStateException] should be thrownBy basketContainer.calculateBasketTotal()
        val exception = the [IllegalStateException] thrownBy basketContainer.calculateBasketTotal()
        exception.getMessage shouldBe "Could not calculate the basket price for A"
      }

      "basket has a negative price for an item and negative quantity" in {
        val basketContainer = new BasketContainer(Map(StoreKeepingUnit("A", -10, None) -> -1))
        an[IllegalStateException] should be thrownBy basketContainer.calculateBasketTotal()
        val exception = the [IllegalStateException] thrownBy basketContainer.calculateBasketTotal()
        exception.getMessage shouldBe "Could not calculate the basket price for A"
      }
    }

  }

  "addToBasket" should {

    val itemA = StoreKeepingUnit("A", 10, None)
    val itemB = StoreKeepingUnit("B", 10, None)

    "add new item to basket if it does not already exist" when {
      "basket is empty" in {

        val basketContainer = BasketContainer(Map.empty)
        basketContainer.addToBasket(itemA)

        basketContainer.basket.size shouldBe 1
        basketContainer.basket should contain (itemA -> 1)
      }

      "basket is not empty" in {
        val basketContainer = BasketContainer(Map(itemA -> 1))
        basketContainer.addToBasket(itemB)

        basketContainer.basket.size shouldBe 2
        basketContainer.basket should contain (itemA -> 1)
        basketContainer.basket should contain (itemB -> 1)
      }
    }

    "should add additional item to basket when it already exists" when {

      "it is the only item type in the basket" in {
        val basketContainer = BasketContainer(Map(itemA -> 3))
        basketContainer.addToBasket(itemA)

        basketContainer.basket.size shouldBe 1
        basketContainer.basket should contain (itemA -> 4)
      }

      "there are multiple types of other items in the basket" in {
        val basketContainer = BasketContainer(Map(itemA -> 3, itemB -> 4))
        basketContainer.addToBasket(itemB)

        basketContainer.basket.size shouldBe 2
        basketContainer.basket should contain (itemA -> 3)
        basketContainer.basket should contain (itemB -> 5)
      }
    }

  }

}
