package kata.kata20

sealed trait Pile {
  def cards: List[Card]
  def shortDescription:String
}

case class FoundationPile(val index: Int, override val cards: List[Card]) extends Pile() {
  def shortDescription:String = "f" + index
}

object FoundationPile {
  def apply(index: Int, cards: Card*): FoundationPile = FoundationPile(index, cards.toList)
}

case class TableauPile(val index: Int, override val cards: List[Card]) extends Pile() {
   def shortDescription:String = "t" + index
}

object TableauPile {
  def apply(index: Int, cards: Card*): TableauPile = TableauPile(index, cards.toList)
}

case class DiscardPile(override val cards: List[Card]) extends Pile(){
  def shortDescription:String = "d" 
}

object DiscardPile {
  def apply(cards: Card*): DiscardPile = DiscardPile(cards.toList)
}

case class StockPile(override val cards: List[Card]) extends Pile(){
  def shortDescription:String = "s" 
}

object StockPile {
  def apply(cards: Card*): StockPile = StockPile(cards.toList)
}