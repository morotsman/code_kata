package kata.kata20

import org.scalatest._

class KlondikeRulesSpec extends FlatSpec with Matchers {


  var foundationPile1 = new FoundationPile(List());
  var foundationPile2 = new FoundationPile(List());
  var foundationPile3 = new FoundationPile(List());
  var foundationPile4 = new FoundationPile(List());
  var tableauPile1 = new TableauPile(List());
  var tableauPile2 = new TableauPile(List());
  var tableauPile3 = new TableauPile(List());
  var tableauPile4 = new TableauPile(List());
  var tableauPile5 = new TableauPile(List());
  var tableauPile6 = new TableauPile(List());
  var tableauPile7 = new TableauPile(List());
  var stockPile = new StockPile(List());
  var discardPile = new DiscardPile(List())

  "If an stock pile is empty it" should "be possible to move all cards in the discard pile to the stock pile" in {

    var discardPile = new DiscardPile(List())
    var board = KlondikeBoard(stockPile, discardPile, List(tableauPile1, tableauPile2, tableauPile3, tableauPile4, tableauPile5, tableauPile6, tableauPile7), List(foundationPile1, foundationPile2, foundationPile3, foundationPile4))
    board.legalMoves().length should be(0)

    discardPile = new DiscardPile(List(Card(Suite.Clubs, 2, true)))
    board = KlondikeBoard(stockPile, discardPile, List(tableauPile1, tableauPile2, tableauPile3, tableauPile4, tableauPile5, tableauPile6, tableauPile7), List(foundationPile1, foundationPile2, foundationPile3, foundationPile4))
    board.legalMoves().length should be(1)
    board.legalMoves() should be(List(Move(discardPile, stockPile, 1)))

    discardPile = new DiscardPile(List(Card(Suite.Clubs, 2, true), Card(Suite.Clubs, 3, true), Card(Suite.Clubs, 4, true)))
    board = KlondikeBoard(stockPile, discardPile, List(tableauPile1, tableauPile2, tableauPile3, tableauPile4, tableauPile5, tableauPile6, tableauPile7), List(foundationPile1, foundationPile2, foundationPile3, foundationPile4))
    board.legalMoves().length should be(1)
    board.legalMoves() should be(List(Move(discardPile, stockPile, 3)))
  }

}