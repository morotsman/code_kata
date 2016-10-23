package kata.kata20

import GameEngine.Turn

case class Result()

case class Move(val from: Pile, val to: Pile, val numberOfCards: Int)

case class Card(val suite: Suite.Value, val value: Int, val hidden: Boolean)

object Suite extends Enumeration {
  val Hearts, Spades, Diamonds, Clubs = Value
}


trait Board {
  def legalMoves(): List[Move];
  
  def makeMove(move: Move): Board;
}

case class Game(val board: Board) {
  def legalMoves(): List[Move] =
    board.legalMoves()
}


case class KlondikeBoard(val stockPile: StockPile, discardPile: DiscardPile, tableauPiles : List[TableauPile], foundationPiles: List[FoundationPile]) extends Board {
  

  private def stockIsEmpty: List[Move] =
    if (stockPile.cards.length == 0 && discardPile.cards.length > 0) {
      List(Move(discardPile, stockPile, discardPile.cards.length))
    } else {
      List()
    }
 
  
  private def takeCardFromStock: List[Move] = 
    if(stockPile.cards.length> 0) {
      List(Move(stockPile, discardPile, 1))
    } else {
      List()
    }
    
  private def moveToFoundationPile: List[Move] = {
    
    def suiteMatches(foundationTopCard: Option[Card], card: Card): Boolean = 
      foundationTopCard.map(_.suite == card.suite).getOrElse(true)
      
    def isNextCard(foundationTopCard: Option[Card], card: Card): Boolean = 
      foundationTopCard.map(_.value).getOrElse(0) == card.value - 1
    
    def go(piles: List[(FoundationPile, Option[Card])]): List[Move] = piles match {
      case Nil => List()  
      case (p, topCard)::ps => {
        val moves = for(
          pile <- (discardPile::tableauPiles)
          if (pile.cards.length > 0 && !pile.cards.head.hidden && suiteMatches(topCard, pile.cards.head) && isNextCard(topCard, pile.cards.head))
        ) yield Move(pile,p,1)
        moves ::: go(ps)
      }
    } 
    go(foundationPiles.map(p => (p,p.cards.headOption)))
  }

  def legalMoves(): List[Move] = 
    stockIsEmpty ::: takeCardFromStock ::: moveToFoundationPile
  
  def makeMove(move: Move): Board = ???

}

abstract class Pile(val cards: List[Card]) 

class FoundationPile(override val cards: List[Card]) extends Pile(cards)

class TableauPile(override val cards: List[Card]) extends Pile(cards)

class DiscardPile(override val cards: List[Card]) extends Pile(cards)

class StockPile(override val cards: List[Card]) extends Pile(cards)

case object KlondikeBoard


case class GameEngine(val run: Turn)

object GameEngine {
  type Turn = Game => (Result, Option[Game])

  def takeTurn(move: Move): GameEngine = GameEngine(
    (game: Game) => {
      val availableMoves = game.legalMoves()
      if (availableMoves.contains(move)) {
        val updatedGame = ???
        (Result(), Some(updatedGame))
      } else {
        (Result(), None)
      }

    })

}

object KlondikeBoardGenerator {

  val cards = generateSuite(Suite.Clubs) ::: generateSuite(Suite.Diamonds) ::: generateSuite(Suite.Hearts) ::: generateSuite(Suite.Spades)

  def generateSuite(suite: Suite.Value): List[Card] =
    (1 to 13).toList.map(v => Card(suite, v, true))

  def shuffelCards(cards: List[Card]) =
    scala.util.Random.shuffle(cards)
    
  def getTableauPiles(source: List[Card], nrOfPiles: Int):List[TableauPile] = {
    if (nrOfPiles == 0) {
      return List()
    } else {
      val cardsForPile = source.take(nrOfPiles).zipWithIndex.map(c => if(c._2 == 0) Card(c._1.suite,c._1.value,false) else c._1)
      val cardsLeft = source.drop(nrOfPiles)
      new TableauPile(cardsForPile) :: getTableauPiles(cardsLeft, nrOfPiles-1)
    }
  }

  def generate: KlondikeBoard = {
    val randomCards = shuffelCards(cards)
    val foundationPiles = (1 to 4).map(i => new FoundationPile(List())).toList
    val tableauPiles = getTableauPiles(randomCards,7)
    val discardPile = new DiscardPile(List())
    val stockPile = new StockPile(randomCards.drop(28))
    KlondikeBoard(stockPile, discardPile, tableauPiles, foundationPiles)
  }

}

object Klondike {

  def playGame(game: Game): Unit = {
    println(game.board)

    //get move from user
    val moveFromUser = ???

    val (result, newGame) = GameEngine.takeTurn(moveFromUser).run(game)

    if (newGame == None) {
      println("Wrong move")
      playGame(game)
    } else {
      playGame(newGame.get)
    }

  }

  def main(args: Array[String]) {
    val game = Game(KlondikeBoardGenerator.generate)

    playGame(game)

  }

}
