package kata.kata20

import org.scalatest._

class KlondikeGeneratorSpec extends FlatSpec with Matchers {
 
  
  def validateTableuPile(nr: Int): Unit = {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    val pile = board.tableauPiles.filter(_.cards.length == nr)
    pile.length should be(1)
    pile.head.cards.head.hidden should be(false)
    pile.head.cards.drop(1).forall(_.hidden) should be(true)
  }
  
  "A valid klondike board" should "contain 1 card in a tableuPile and the first card should be open" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    validateTableuPile(1)
  }  

  "A valid klondike board" should "contain 2 card in a tableuPile and the first card should be open" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    validateTableuPile(2)
  } 
  
  "A valid klondike board" should "contain 3 card in a tableuPile and the first card should be open" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    validateTableuPile(3)
  }   
  
  "A valid klondike board" should "contain 4 card in a tableuPile and the first card should be open" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    validateTableuPile(4)
  } 
  
  "A valid klondike board" should "contain 5 card in a tableuPile and the first card should be open" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    validateTableuPile(5)
  }    
  
  "A valid klondike board" should "contain 6 card in a tableuPile and the first card should be open" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    validateTableuPile(6)
  }   
  
  "A valid klondike board" should "contain 7 card in a tableuPile and the first card should be open" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    validateTableuPile(7)
  } 
  
  "A valid klondike board" should "contain 24 cards in the stock pile" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.stockPile.cards.length should be(24)
    board.stockPile.cards.head.hidden should be(true)
  }   
  
  "A valid klondike board" should "contain 0 cards in the discard pile" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.discardPile.cards.length should be(0)
  } 
  
  "A valid klondike board" should "contain 0 cards in the foundation piles" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.foundationPiles.forall(_.cards.length == 0) should be(true)
  }    
  
  /****************Rules*****************/
  
  "An empty stock pile" should "give a valid move to move all cards in the discard pile to the stock pile" in {
    val foundationPile1 = new FoundationPile(List());
    val foundationPile2 = new FoundationPile(List());
    val foundationPile3 = new FoundationPile(List());
    val foundationPile4 = new FoundationPile(List());
    val tableauPile1 = new TableauPile(List());
    val tableauPile2 = new TableauPile(List());
    val tableauPile3 = new TableauPile(List());
    val tableauPile4 = new TableauPile(List());
    val tableauPile5 = new TableauPile(List());
    val tableauPile6 = new TableauPile(List());
    val tableauPile7 = new TableauPile(List());
    val stockPile = new StockPile(List());
    
    var discardPile = new DiscardPile(List())
    var board = KlondikeBoard(stockPile,discardPile,List(tableauPile1,tableauPile2,tableauPile3,tableauPile4,tableauPile5,tableauPile6,tableauPile7),List(foundationPile1, foundationPile2, foundationPile3, foundationPile4))        
    board.legalMoves().length should be(0)
    
    discardPile = new DiscardPile(List(Card(Suite.Clubs, 2, true)))
    board = KlondikeBoard(stockPile,discardPile,List(tableauPile1,tableauPile2,tableauPile3,tableauPile4,tableauPile5,tableauPile6,tableauPile7),List(foundationPile1, foundationPile2, foundationPile3, foundationPile4))        
    board.legalMoves().length should be(1)
    board.legalMoves() should be(List(Move(discardPile,stockPile,1)))
    
    discardPile = new DiscardPile(List(Card(Suite.Clubs, 2, true),Card(Suite.Clubs, 3, true),Card(Suite.Clubs, 4, true)))
    board = KlondikeBoard(stockPile,discardPile,List(tableauPile1,tableauPile2,tableauPile3,tableauPile4,tableauPile5,tableauPile6,tableauPile7),List(foundationPile1, foundationPile2, foundationPile3, foundationPile4))        
    board.legalMoves().length should be(1)
    board.legalMoves() should be(List(Move(discardPile,stockPile,3)))    
  }   
  

}