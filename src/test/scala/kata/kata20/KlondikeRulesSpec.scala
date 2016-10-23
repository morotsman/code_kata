package kata.kata20

import org.scalatest._

class KlondikeRulesSpec extends FlatSpec with Matchers {

  def cards(numberOfCards: Int): List[Card] =
    if (numberOfCards == 0) List()
    else Card(Suite.Clubs, 2, true) :: cards(numberOfCards - 1)

  /**
   * If the Stock becomes empty, turn the entire discard pile over and make it the new Stock.
   */
  "If an stock pile is empty it" should "be possible to move all cards in the discard pile to the stock pile" in {
    var stockPile = new StockPile(cards(0))

    var discardPile = new DiscardPile(cards(0))
    var board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).build
    board.legalMoves().length should be(0)

    discardPile = new DiscardPile(cards(1))
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).build
    board.legalMoves().length should be(1)
    board.legalMoves() should be(List(Move(discardPile, stockPile, 1)))

    discardPile = new DiscardPile(cards(3))
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).build
    board.legalMoves().length should be(1)
    board.legalMoves() should be(List(Move(discardPile, stockPile, 3)))
  }

  /**
   * Turn over the top card of the Stock and place it face-up on the Discard pile.
   */
  "If the stock pile has cards" should "be possible to move a card from the stock pile to the discard pile" in {
    var discardPile = new DiscardPile(cards(0))

    var stockPile = new StockPile(cards(0))
    var board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).build
    board.legalMoves().length should be(0)

    stockPile = new StockPile(cards(1));
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).build
    board.legalMoves().length should be(1)
    board.legalMoves() should be(List(Move(stockPile, discardPile, 1)))

    stockPile = new StockPile(cards(3));
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).build
    board.legalMoves().length should be(1)
    board.legalMoves() should be(List(Move(stockPile, discardPile, 1)))

  }

  def emptyFoundationsPiles: List[FoundationPile] = List(new FoundationPile(cards(0)), new FoundationPile(cards(0)), new FoundationPile(cards(0)), new FoundationPile(cards(0)))

  def emptyTableauPiles: List[TableauPile] = List(new TableauPile(cards(0)), new TableauPile(cards(0)), new TableauPile(cards(0)), new TableauPile(cards(0)), new TableauPile(cards(0)), new TableauPile(cards(0)), new TableauPile(cards(0)))

  /**
   * Move a card from the tableau or discard pile to one of the foundation piles.
   * If the foundation pile is empty, only an Ace can be placed there, otherwise only the next highest card in the appropriate suit can be placed
   * (so if a foundation pile is currently showing a four of hearts, only the five of hearts may be placed there).
   */
  "If an foundation pile is empty it" should "be possible to move a ace from the tableau or the discard pile" in {
    var stockPile = StockPile(Card(Suite.Hearts, 1, true))

    var discardPile = new DiscardPile(cards(0))
    var board = BoardBuilder().withDiscardPile(discardPile).withTableauPiles(emptyTableauPiles).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(0)

    discardPile = DiscardPile(Card(Suite.Clubs, 1, false))
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).withTableauPiles(emptyTableauPiles).withFoundationPiles(emptyFoundationsPiles).build
    println(board.legalMoves())
    board.legalMoves().length should be(5)

    discardPile = DiscardPile(Card(Suite.Clubs, 2, false))
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).withTableauPiles(emptyTableauPiles).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(1)
  }

  "If an foundation pile has a card" should "be possible to move a card from the tableau or the discard pile if it has the same Suite an the next number in rank" in {
    var stockPile = StockPile(Card(Suite.Hearts, 1, true))

    var discardPile = DiscardPile(Card(Suite.Clubs, 2, false))
    val foundationPileWithClubAce = FoundationPile(Card(Suite.Clubs, 1, false))
    var board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).withTableauPiles(emptyTableauPiles).withFoundationPile(0, foundationPileWithClubAce).build
    board.legalMoves().length should be(2)
    board.legalMoves() should be(List(Move(stockPile, discardPile, 1), Move(discardPile, foundationPileWithClubAce, 1)))

    discardPile = DiscardPile(Card(Suite.Clubs, 3, false))
    val foundationPileWithClubTwo = FoundationPile(Card(Suite.Clubs, 2, false), Card(Suite.Clubs, 1, false))
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).withTableauPiles(emptyTableauPiles).withFoundationPile(0, foundationPileWithClubTwo).build
    board.legalMoves().length should be(2)
    board.legalMoves() should be(List(Move(stockPile, discardPile, 1), Move(discardPile, foundationPileWithClubTwo, 1)))

    discardPile = DiscardPile(Card(Suite.Hearts, 2, false))
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).withTableauPiles(emptyTableauPiles).withFoundationPile(0, new FoundationPile(List(Card(Suite.Clubs, 1, true)))).build
    board.legalMoves().length should be(1)

    discardPile = DiscardPile(Card(Suite.Clubs, 3, false))
    board = BoardBuilder().withStockPile(stockPile).withDiscardPile(discardPile).withTableauPiles(emptyTableauPiles).withFoundationPile(0, new FoundationPile(List(Card(Suite.Clubs, 1, true)))).build
    board.legalMoves().length should be(1)

    discardPile = DiscardPile(Card(Suite.Clubs, 2, false))
    val tableauPile = TableauPile(Card(Suite.Diamonds, 2, false))
    val foundationPileWithDiamondAce = FoundationPile(Card(Suite.Diamonds, 1, false))
    board = BoardBuilder().withStockPile(stockPile).
      withDiscardPile(discardPile).
      withTableauPile(3, tableauPile).
      withFoundationPile(0, foundationPileWithClubAce).
      withFoundationPile(1, foundationPileWithDiamondAce).
      build
    board.legalMoves().length should be(4)
    board.legalMoves() should be(List(Move(stockPile, discardPile, 1), Move(discardPile, foundationPileWithClubAce, 1),
      Move(tableauPile, foundationPileWithDiamondAce, 1), Move(foundationPileWithClubAce, tableauPile, 1)))
  }

  /**
   *  Move the top card of the discard pile or a foundation pile to one of the tableau piles.
   *  This card must be one less in rank and opposite in color to the card at the top of the destination tableau.
   */

  "If tableau has a card it" should "be possible to move a vard from the tableau or the discard pile if it is in the opposite color and one less in rank" in {
    var stockPile = StockPile(cards(0))
    var discardPile = DiscardPile(cards(0))
    var board = BoardBuilder().withDiscardPile(discardPile).withTableauPiles(emptyTableauPiles).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(0)

    var tableauPile = TableauPile(Card(Suite.Clubs, 9, false), Card(Suite.Hearts, 10, false))
    discardPile = DiscardPile(Card(Suite.Diamonds, 8, false))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(1, tableauPile).withFoundationPiles(emptyFoundationsPiles).build
    println(board.legalMoves())
    board.legalMoves().length should be(2)
    
    tableauPile = TableauPile(Card(Suite.Diamonds, 9, false), Card(Suite.Hearts, 10, false))
    discardPile = DiscardPile(Card(Suite.Spades, 8, false))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(2, tableauPile).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(2)

    tableauPile = TableauPile(Card(Suite.Diamonds, 9, false), Card(Suite.Hearts, 10, false))
    discardPile = DiscardPile(Card(Suite.Diamonds, 8, false))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(1, tableauPile).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(1)

    tableauPile = TableauPile(Card(Suite.Diamonds, 9, false), Card(Suite.Hearts, 10, false))
    discardPile = DiscardPile(Card(Suite.Hearts, 8, false))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(3, tableauPile).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(1)
    
    
    tableauPile = TableauPile(Card(Suite.Hearts, 9, false), Card(Suite.Clubs, 10, false))
    discardPile = DiscardPile(Card(Suite.Clubs, 8, false))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(4, tableauPile).withFoundationPiles(emptyFoundationsPiles).build
    println(board.legalMoves())
    board.legalMoves().length should be(2)
    
    tableauPile = TableauPile(Card(Suite.Hearts, 9, false), Card(Suite.Hearts, 10, false))
    discardPile = DiscardPile(Card(Suite.Spades, 8, false))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(5, tableauPile).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(2)

    tableauPile = TableauPile(Card(Suite.Hearts, 9, false), Card(Suite.Hearts, 10, false))
    discardPile = DiscardPile(Card(Suite.Diamonds, 8, false))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(6, tableauPile).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(1)

    tableauPile = TableauPile(Card(Suite.Hearts, 9, false), Card(Suite.Hearts, 10, false))
    discardPile = DiscardPile(Card(Suite.Hearts, 8, false))
    board = BoardBuilder().withDiscardPile(discardPile).withTableauPile(0, tableauPile).withFoundationPiles(emptyFoundationsPiles).build
    board.legalMoves().length should be(1)    
    
    
  }

}