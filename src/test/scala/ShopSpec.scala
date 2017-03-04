
import org.scalatest.{Matchers, WordSpec}

class ShopSpec extends WordSpec with Matchers {

  "lookUpAndAddItem" should {

    "add nothing to basket if shop does not contain item" in {
      val basketContainer = new BasketContainer(Map.empty)
      ShopApplication.lookUpAndAddItem("G",basketContainer)
      basketContainer.basket.size  shouldBe 0
    }

    "add item to basket when item exists" in {
      val basketContainer = new BasketContainer(Map.empty)
      ShopApplication.lookUpAndAddItem("A", basketContainer)
      basketContainer.basket.size shouldBe 1
    }

  }

  "validateNewUnitPrice" should {

    val itemA = Store.itemsToSell.head

    "not update the sku price" when {

      "if new unit price is empty" in {
        val shopBefore = Store.itemsToSell
        val input = ""
        ShopApplication.validateNewUnitPrice(itemA, input)
        Store.itemsToSell.find(_.name == itemA.name).map(_.unitPrice) shouldBe Some(itemA.unitPrice)
      }


      "if new unit price is not parseable as int" in {
        val shopBefore = Store.itemsToSell
        val input = "IHIS*()£@($)(984u2"
        an [NumberFormatException] should be thrownBy ShopApplication.validateNewUnitPrice(itemA, input)
        val exception = the [NumberFormatException] thrownBy ShopApplication.validateNewUnitPrice(itemA, input)
        exception.getMessage shouldBe s"Could not parse $input as a valid price in pence"
      }

    }

    "update the sku price" when {

      "the new price is zero" in {
        val input = "0"
        ShopApplication.validateNewUnitPrice(itemA, input)
        Store.itemsToSell.find(_.name == itemA.name).map(_.unitPrice) shouldBe Some(0)
      }

      "the new price is more than zero" in {
        val input = "300"
        ShopApplication.validateNewUnitPrice(itemA, input)
        Store.itemsToSell.find(_.name == itemA.name).map(_.unitPrice) shouldBe Some(300)
      }

    }



  }




}












