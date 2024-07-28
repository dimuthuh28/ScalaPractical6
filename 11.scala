case class Product(id: Int, name: String, quantity: Int, price: Double)

type Inventory = Map[Int, Product]

def retrieveProductNames(inventory: Inventory): Seq[String] = {
  inventory.values.map(_.name).toSeq
}

def calculateTotalValue(inventory: Inventory): Double={
  inventory.values.map(a => a.price* a.quantity).sum
}

def isEmpty(inventory: Inventory):Boolean = inventory.isEmpty

def mergeInventories(inventory1: Inventory, inventory2:Inventory): Inventory={
  inventory1 ++ inventory2.map{case (id, product2)=>id->inventory1.get(id).map{
    product1 => Product(id, product2.name, product1.quantity + product2.quantity, math.max(product1.price, product2.price))
  }.getOrElse(product2)}
}

def getProductDetails(inventory: Inventory, productId: Int): Option[Product] = {
  inventory.get(productId)
}

def printProductDetails(inventory: Inventory, productId: Int): Unit = {
  getProductDetails(inventory, productId) match {
    case Some(product) =>
      println(s"Product ID: ${product.id}, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}")
    case None =>
      println(s"Product with ID $productId not found")
  }
}

object InventoryManagement extends App{
  var inventory1:Inventory = Map(
    51 -> Product(51, "A", 20, 5.0),
    52 -> Product(52, "B", 23, 7.5)
  )

  var inventory2:Inventory = Map(
    52 -> Product(52, "B", 22, 7.5),
    53 -> Product(53, "C", 12, 10.0)
  )  

  val productNames = retrieveProductNames(inventory1)
  println(productNames)

  val totalValue = calculateTotalValue(inventory1)
  println(s"Total value: $totalValue")

  println(s"Is Inventory 1 empty: ${isEmpty(inventory1)}.");

  var mergedInventory = mergeInventories(inventory1,inventory2) 
  println(mergedInventory)

  println(printProductDetails(inventory1, 52))
  println(printProductDetails(inventory1, 59))

}