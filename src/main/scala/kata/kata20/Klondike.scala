package kata.kata20

import GameEngine.Turn

case class Result()

case class Move(val from: Pile, val to: Pile, val numberOfCards: Int)

case class Card(val suite: Suite.Value, val value: Int, val hidden: Boolean) {

  def isBlack: Boolean = suite == Suite.Clubs || suite == Suite.Spades

  def oppositeColor(that: Card): Boolean =
    (isBlack && !that.isBlack) || (!isBlack && that.isBlack)

  def isPrevCard(card: Card): Boolean =
    value + 1 == card.value

  def isNextCard(card: Card): Boolean =
    value - 1 == card.value

}

object Suite extends Enumeration {
  val Hearts, Spades, Diamonds, Clubs = Value
}

trait Board {
  def legalMoves(): List[Move];

  def makeMove(move: Move): KlondikeBoard;
}

case class Game(val board: KlondikeBoard) {
  def legalMoves: List[Move] =
    board.legalMoves()

  def renderBoard: Unit =
    AsciiBoardRenderer.renderBoard(board)
}

case class KlondikeBoard(val stockPile: StockPile, discardPile: DiscardPile, tableauPiles: List[TableauPile], foundationPiles: List[FoundationPile]) extends Board {

  def legalMoves(): List[Move] = {

    def fillStockIfEmpty: List[Move] =
      if (stockPile.cards.length == 0 && discardPile.cards.length > 0) {
        List(Move(discardPile, stockPile, discardPile.cards.length))
      } else {
        List()
      }

    def takeCardFromStock: List[Move] =
      if (stockPile.cards.length > 0) {
        List(Move(stockPile, discardPile, 1))
      } else {
        List()
      }

    def moveToFoundationPile: List[Move] = {

      def suiteMatches(foundationTopCard: Option[Card], card: Card): Boolean =
        foundationTopCard.map(_.suite == card.suite).getOrElse(true)

      def isNextCard(foundationTopCard: Option[Card], card: Card): Boolean =
        foundationTopCard.map(_.value).getOrElse(0) == card.value - 1

      def go(piles: List[(FoundationPile)]): List[Move] = piles match {
        case Nil => List()
        case (targetPile) :: ps => {
          val moves = for (
            sourcePile <- (discardPile :: tableauPiles);
            if sourcePile.cards.length > 0 && suiteMatches(targetPile.cards.headOption, sourcePile.cards.head) && isNextCard(targetPile.cards.headOption, sourcePile.cards.head)
          ) yield Move(sourcePile, targetPile, 1)
          moves ::: go(ps)
        }
      }
      go(foundationPiles)
    }

    /**
     * Move the top card of the discard pile to one of the tableau piles.
     * This card must be one less in rank and opposite in color to the card at the top of the destination tableau.
     */

    def moveToTableauPile: List[Move] = {

      def go(piles: List[TableauPile]): List[Move] = piles match {
        case Nil => List()
        case targetPile :: ps => {
          val moves = for (
            sourcePile <- (discardPile :: foundationPiles);
            if sourcePile.cards.length > 0 && targetPile.cards.length > 0
              && sourcePile.cards.head.oppositeColor(targetPile.cards.head) && sourcePile.cards.head.isPrevCard(targetPile.cards.head)
          ) yield Move(sourcePile, targetPile, 1)
          moves ::: go(ps)
        }
      }

      go(tableauPiles)
    }

    /**
     * Move one or more cards from one tableau pile to another.
     * If multiple cards are moved, they must be a sequence ascending in rank and alternating in color.
     * The card moved (or the top of the sequence moved) must be one less in rank and opposite in color to the card at the top of the destination tableau.
     * If the move leaves a face-down card to the top of the original pile, turn it over.
     */
    def moveCardFromOneTableauToAnother: List[Move] = {

      def go(piles: List[TableauPile]): List[Move] = piles match {
        case Nil => List()
        case targetPile :: ps => {
          val moves = for (
            sourcePile <- tableauPiles if targetPile != sourcePile && sourcePile.cards.length > 0 && targetPile.cards.length > 0;
            (card: Card, index) <- sourcePile.cards.zipWithIndex if !card.hidden && card.oppositeColor(targetPile.cards.head) && card.isPrevCard(targetPile.cards.head)
          ) yield Move(sourcePile, targetPile, index + 1)
          moves ::: go(ps)
        }
      }

      go(tableauPiles)
    }

    /**
     * If a move leaves a tableau pile empty, an exposed King at the top of a tableau or discard pile,
     * or a sequence starting with a King on a tableau pile, may be moved to it.
     */
    def moveKingToEmptyTableau: List[Move] = {

      def go(piles: List[TableauPile]): List[Move] = piles match {
        case Nil => List()
        case targetPile :: ps => {
          val discardAndFoundationMoves = for (
            sourcePile <- discardPile :: foundationPiles;
            if targetPile.cards.length == 0 && sourcePile.cards.length > 0 && sourcePile.cards.head.value == 13
          ) yield Move(sourcePile, targetPile, 1)

          val tableauMoves = for (
            sourcePile <- tableauPiles if sourcePile != targetPile && targetPile.cards.length == 0 && sourcePile.cards.length > 0;
            (card: Card, index) <- sourcePile.cards.zipWithIndex;
            if !card.hidden && card.value == 13
          ) yield Move(sourcePile, targetPile, index + 1)

          tableauMoves ::: discardAndFoundationMoves ::: go(ps)
        }

      }

      go(tableauPiles)

    }

    fillStockIfEmpty ::: takeCardFromStock ::: moveToFoundationPile ::: moveToTableauPile ::: moveCardFromOneTableauToAnother ::: moveKingToEmptyTableau
  }

  def makeMove(move: Move): KlondikeBoard = {
    
    def replaceTableauPile(tableauPiles: List[TableauPile],from: TableauPile, to: TableauPile): List[TableauPile] = tableauPiles match {
      case Nil => Nil
      case (t::ts) => (if(t == from) to else t) :: replaceTableauPile(ts, from,to)
    }

    def go(move: Move): KlondikeBoard = move match {
      case Move(StockPile(from), DiscardPile(to), number) =>
        val newStock = StockPile(from.drop(1))
        val cards = from.take(1).map(c => Card(c.suite, c.value, false))
        val newDiscard = DiscardPile(cards ::: to)
        KlondikeBoard(newStock, newDiscard, tableauPiles, foundationPiles)
      case Move(DiscardPile(from), StockPile(to), number) =>
        val cards = from.map(c => Card(c.suite, c.value, true)).reverse
        val newStock = StockPile(cards)
        val newDiscard = DiscardPile(Nil)
        KlondikeBoard(newStock, newDiscard, tableauPiles, foundationPiles)
      case Move(DiscardPile(from), TableauPile(to), number) =>
        val cards = from.take(1)
        val newDiscard = DiscardPile(from.drop(1))
        val newTableau = TableauPile(cards ::: to)
        KlondikeBoard(stockPile, newDiscard, replaceTableauPile(tableauPiles,TableauPile(to), newTableau), foundationPiles)
      case _ => ???
    }

    go(move)

  }

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
      val availableMoves = game.legalMoves
      if (availableMoves.contains(move)) {
        val updatedBoard = game.board.makeMove(move)
        (Result(), Some(Game(updatedBoard)))
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

  def getTableauPiles(source: List[Card], nrOfPiles: Int): List[TableauPile] = {
    if (nrOfPiles == 0) {
      return List()
    } else {
      val cardsForPile = source.take(nrOfPiles).zipWithIndex.map(c => if (c._2 == 0) Card(c._1.suite, c._1.value, false) else c._1)
      val cardsLeft = source.drop(nrOfPiles)
      new TableauPile(cardsForPile) :: getTableauPiles(cardsLeft, nrOfPiles - 1)
    }
  }

  def generate: KlondikeBoard = {
    val randomCards = shuffelCards(cards)
    val foundationPiles = (1 to 4).map(i => new FoundationPile(List())).toList
    val tableauPiles = getTableauPiles(randomCards, 7)
    val discardPile = new DiscardPile(List())
    val stockPile = new StockPile(randomCards.drop(28))
    KlondikeBoard(stockPile, discardPile, tableauPiles, foundationPiles)
  }

}

object Klondike {

  def stringToMove(b: KlondikeBoard, moveAsString: String): Option[Move] = {
    
    def tableauPile(nr: Int): TableauPile = 
      b.tableauPiles.drop(nr-1).head
    
    
    moveAsString.split(" ").toList match {
      case from :: to :: number if from == "s" && to == "d" => Some(Move(b.stockPile, b.discardPile, 1))
      case from :: to :: number if from == "d" && to == "s" => Some(Move(b.discardPile, b.stockPile, b.discardPile.cards.length))
      case from :: to :: number if from == "d" && to.startsWith("t") => 
        if(to.drop(1) != "" && to.drop(1).forall(Character.isDigit)){
           Some(Move(b.discardPile, tableauPile(to.drop(1).toInt), 1)) 
        } else {
          None
        }
      case _ => None
    }
  }
  

  def moveToString(b: KlondikeBoard, move: Move): String = move match {
    case Move(DiscardPile(_), StockPile(_), number)      => "DiscardPile" + " => " + "StockPile" + ": " + number
    case Move(DiscardPile(_), FoundationPile(_), number) => "DiscardPile" + " => " + "FoundationPile" + ": " + number
    case Move(DiscardPile(_), TableauPile(_), number)    => "DiscardPile" + " => " + "TableauPile" + (b.tableauPiles.indexOf(move.to)+1) + ": " + number
    case Move(StockPile(_), DiscardPile(_), number)      => "StockPile" + " => " + "DiscardPile" + ": " + number
    case Move(TableauPile(_), TableauPile(_), number)    => "TableauPile" + " => " + "TableauPile" + ": " + number
    case Move(TableauPile(_), FoundationPile(_), number) => "TableauPile" + " => " + "FoundationPile" + ": " + number
    case Move(FoundationPile(_), TableauPile(_), number) => "TableauPile" + " => " + "FoundationPile" + ": " + number
  }

  def playGame(game: Game): Unit = {
    game.renderBoard
    println(game.board.legalMoves.map(m => moveToString(game.board,m)))

    //get move from user
    val moveFromUser = scala.io.StdIn.readLine("Make move>")
    println(moveFromUser)
    val move = stringToMove(game.board, moveFromUser)
    
    val newGame = move.flatMap(m => GameEngine.takeTurn(m).run(game)._2)
    

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
