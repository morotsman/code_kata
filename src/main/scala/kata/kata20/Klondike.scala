package kata.kata20

import GameEngine.Turn

case class Result()

case class Move(val from: Pile, val to: Pile, val numberOfCards: Int)

case class Card(val suite: Suite.Value, val value: Int, val hidden: Boolean) {
  
  def isBlack: Boolean = suite == Suite.Clubs || suite == Suite.Spades 
  
  def oppositeColor(that: Card): Boolean = 
    (this.isBlack && !that.isBlack) || (!this.isBlack && that.isBlack)
  
}

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
  

  private def fillStockIfEmpty: List[Move] =
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
      case (fp, topCard)::ps => {
        val moves = for(
          pile <- (discardPile::tableauPiles)
          if pile.cards.length > 0 && suiteMatches(topCard, pile.cards.head) && isNextCard(topCard, pile.cards.head)
        ) yield Move(pile,fp,1)
        moves ::: go(ps)
      }
    } 
    go(foundationPiles.map(p => (p,p.cards.headOption)))
  }
  
  /**
   Move the top card of the discard pile to one of the tableau piles. 
   This card must be one less in rank and opposite in color to the card at the top of the destination tableau.
   */
  
  def moveToTableauPile: List[Move] = {
      
    def oppositeColor(card1: Card, card2: Card): Boolean =
      card1.oppositeColor(card2)
   
      
       
    def isPrevCard(card1: Card, card2: Card): Boolean = 
      card1.value + 1 == card2.value
   
      
    
    def go(tableauPiles: List[TableauPile]): List[Move] = tableauPiles match {
      case Nil => List()
      case tp::ps => {
        val moves = for(
          pile <- (discardPile::foundationPiles) 
          if pile.cards.length > 0 && tp.cards.length > 0 && oppositeColor(pile.cards.head, tp.cards.head) && isPrevCard(pile.cards.head, tp.cards.head)
        ) yield Move(pile,tp,1)
        moves ::: go(ps)
      }
    }
    
    go(tableauPiles) 
  }

  def legalMoves(): List[Move] = 
    fillStockIfEmpty ::: takeCardFromStock ::: moveToFoundationPile ::: moveToTableauPile
  
  def makeMove(move: Move): Board = ???

}

sealed trait Pile {
  def cards: List[Card]
}

case class FoundationPile(override val cards: List[Card]) extends Pile()

object FoundationPile {
  def apply(cards: Card*): FoundationPile = FoundationPile(cards.toList)
}

case class TableauPile(override val cards: List[Card]) extends Pile()

object TableauPile {
  def apply(cards: Card*): TableauPile = TableauPile(cards.toList)
}

case class DiscardPile(override val cards: List[Card]) extends Pile()

object DiscardPile {
  def apply(cards: Card*): DiscardPile = DiscardPile(cards.toList)
}

case class StockPile(override val cards: List[Card]) extends Pile()

object StockPile {
  def apply(cards: Card*): StockPile = StockPile(cards.toList)
}

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
