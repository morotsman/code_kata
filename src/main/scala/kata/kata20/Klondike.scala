package kata.kata20

import GameEngine.Turn

case class Result()

case class Move(val from: Pile, val to: Pile, numberOfCards: Int)

case class Card(val suite: Suite.Value, val value: Int, val hidden: Boolean)

object Suite extends Enumeration {
  val Hearts, Spades, Diamonds, Clubs = Value
}

object Piles extends Enumeration {
  val StockPile, DiscardPile, FoundationPile, TableauPile = Value
}

trait Board {
  def legalMoves(): List[Move];
  
  def makeMove(move: Move): Board;
}

case class Game(val board: Board) {
  def legalMoves(): List[Move] =
    board.legalMoves()
}


case class KlondikeBoard(val piles: List[Pile]) extends Board {

  private def stockIsEmpty: List[Move] =
  {
    val stockPile = piles.find(_.pileType == Piles.StockPile).get
    val discardPile = piles.find(_.pileType == Piles.DiscardPile).get
    if (stockPile.cards.length == 0 && discardPile.cards.length > 0) {
      List(Move(discardPile, stockPile, discardPile.cards.length))
    } else {
      List()
    }
  }
    

  def legalMoves(): List[Move] = 
    stockIsEmpty
  
  def makeMove(move: Move): Board = {
    val fromPileCards = move.from.cards.drop(move.numberOfCards)
    val toPileCards = move.from.cards.take(move.numberOfCards) ::: move.to.cards 
    val newFromPile = Pile(move.from.pileType, fromPileCards)
    val newToPile = Pile(move.to.pileType, toPileCards)
    KlondikeBoard(newFromPile:: newFromPile :: piles.filter(p => (p != move.from) && (p != move.to)))
  }
}

case class Pile(val pileType: Piles.Value, val cards: List[Card])

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
    (2 to 14).toList.map(v => Card(suite, v, true))

  def shuffelCards(cards: List[Card]) =
    scala.util.Random.shuffle(cards)
    
  def getTableauPiles(source: List[Card], nrOfPiles: Int):List[Pile] = {
    if (nrOfPiles == 0) {
      return List()
    } else {
      val cardsForPile = source.take(nrOfPiles).zipWithIndex.map(c => if(c._2 == 0) Card(c._1.suite,c._1.value,false) else c._1)
      val cardsLeft = source.drop(nrOfPiles)
      Pile(Piles.TableauPile, cardsForPile) :: getTableauPiles(cardsLeft, nrOfPiles-1)
    }
  }

  def generate: KlondikeBoard = {
    val randomCards = shuffelCards(cards)
    val foundationPiles = (1 to 4).map(i => Pile(Piles.FoundationPile, List())).toList
    val tableauPiles = getTableauPiles(randomCards,7)
    val discardPile = Pile(Piles.DiscardPile, List())
    val stockPile = Pile(Piles.StockPile, randomCards.drop(28))
    KlondikeBoard(List(discardPile, stockPile) ::: foundationPiles ::: tableauPiles)
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
