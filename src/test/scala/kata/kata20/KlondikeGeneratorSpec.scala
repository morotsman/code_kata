package kata.kata20

import org.scalatest._

class KlondikeGeneratorSpec extends FlatSpec with Matchers {


  "A valid klondike board" should "contain 52 cards" in {
   
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    (board.discardPile.length + board.stockPile.length + board.foundationPile1.length + board.foundationPile2.length + board.foundationPile3.length
    + board.foundationPile4.length + board.tableauPile1.length + board.tableauPile2.length + board.tableauPile3.length + board.tableauPile4.length
    + board.tableauPile5.length + board.tableauPile6.length + board.tableauPile7.length) should be(52)
   
  }
  
  "A valid klondike board" should "contain 1 card in tableuPile1" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.tableauPile1.length should be(1) 
  }  

  "A valid klondike board" should "contain 2 cards in tableuPile2" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.tableauPile2.length should be(2) 
  } 
  
  "A valid klondike board" should "contain 3 cards in tableuPile2" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.tableauPile3.length should be(3) 
  }   
  
  "A valid klondike board" should "contain 4 cards in tableuPile2" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.tableauPile4.length should be(4) 
  } 
  
  "A valid klondike board" should "contain 5 cards in tableuPile2" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.tableauPile5.length should be(5) 
  }    
  
  "A valid klondike board" should "contain 6 cards in tableuPile2" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.tableauPile6.length should be(6) 
  }   
  
  "A valid klondike board" should "contain 7 cards in tableuPile2" in {
    val board:KlondikeBoard = KlondikeBoardGenerator.generate
    board.tableauPile7.length should be(7) 
  }     
  

  

}