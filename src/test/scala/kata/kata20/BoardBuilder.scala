package kata.kata20



case class BoardBuilder(
    val discardPile: DiscardPile = new DiscardPile(List()),
    val stockPile: StockPile = new StockPile(List()),
    val foundationPiles : List[FoundationPile] = List(FoundationPile(1,List()), FoundationPile(2,List()), FoundationPile(3,List()), FoundationPile(4,List())),
    val tableauPiles : List[TableauPile] = List(TableauPile(1,List()), TableauPile(2,List()), TableauPile(3,List()), TableauPile(4,List()), TableauPile(5,List()), TableauPile(6,List()), TableauPile(7,List()))

    ) {
  def build: Board =
    KlondikeBoard(stockPile, discardPile, tableauPiles, foundationPiles)
    
  def withStockPile(pile: StockPile): BoardBuilder = BoardBuilder(discardPile,pile, foundationPiles, tableauPiles)

  def withDiscardPile(pile: DiscardPile): BoardBuilder = BoardBuilder(pile,stockPile, foundationPiles, tableauPiles)
  

  def withFoundationPile(nr: Int, pile: FoundationPile): BoardBuilder =  
    BoardBuilder(discardPile,stockPile, foundationPiles.patch(nr, Seq(pile), 1), tableauPiles)

  
  def withFoundationPiles(foundations: List[FoundationPile]): BoardBuilder = 
    BoardBuilder(discardPile, stockPile, foundations, tableauPiles)

  def withTableauPile(nr: Int, pile: TableauPile): BoardBuilder = 
    BoardBuilder(discardPile, stockPile, foundationPiles, tableauPiles.patch(nr, Seq(pile), 1))

  def withTableauPiles(tableaus: List[TableauPile]): BoardBuilder = 
    BoardBuilder(discardPile, stockPile, foundationPiles, tableaus)
  
}
