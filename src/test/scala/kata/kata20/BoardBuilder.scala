package kata.kata20



case class BoardBuilder(
    val discardPile: DiscardPile = new DiscardPile(List()),
    val stockPile: StockPile = new StockPile(List()),
    val foundationPiles : List[FoundationPile] = List(new FoundationPile(List()), new FoundationPile(List()), new FoundationPile(List()), new FoundationPile(List())),
    val tableauPiles : List[TableauPile] = List(new TableauPile(List()), new TableauPile(List()), new TableauPile(List()), new TableauPile(List()), new TableauPile(List()), new TableauPile(List()), new TableauPile(List()))

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
