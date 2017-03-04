import scala.collection.mutable

case class SpecialPrice(offerQuantity: Int, offerPrice: Int)
case class StoreKeepingUnit(name: String, unitPrice: Int, specialPrice: Option[SpecialPrice])

object Store {

  var itemsToSell: scala.collection.mutable.MutableList[StoreKeepingUnit] =
    mutable.MutableList(
      StoreKeepingUnit("A", 50, Some(SpecialPrice(3,130))),
      StoreKeepingUnit("B", 30, Some(SpecialPrice(2,45))),
      StoreKeepingUnit("C", 20, None),
      StoreKeepingUnit("D", 15, None)
    )

}
