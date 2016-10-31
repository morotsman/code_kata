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
      new TableauPile(8-nrOfPiles, cardsForPile) :: getTableauPiles(cardsLeft, nrOfPiles - 1)
    }
  }

  def generate: KlondikeBoard = {
    val randomCards = shuffelCards(cards)
    val foundationPiles = (1 to 4).map(i => new FoundationPile(i,List())).toList
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
    
    def foundationPile(nr: Int): FoundationPile = 
      b.foundationPiles.drop(nr-1).head      
    
    def toInt(s: String): Option[Int] = 
      if(s != "" && s.forall(Character.isDigit)){ 
        Some(s.toInt)
      }else {
        None
      }
    
      
    def getPileIndex(pile: String): Option[Int] = 
        toInt(pile.drop(1))     
    
    
    moveAsString.split(" ").toList match {
      case from :: to :: number if from == "s" && to == "d" => Some(Move(b.stockPile, b.discardPile, 1))
      case from :: to :: number if from == "d" && to == "s" => Some(Move(b.discardPile, b.stockPile, b.discardPile.cards.length))
      case from :: to :: number if from == "d" && to.startsWith("t") => 
        getPileIndex(to).map(i => Move(b.discardPile, tableauPile(i), 1))
      case from :: to :: number if from == "d" && to.startsWith("f") =>
        getPileIndex(to).map(i => Move(b.discardPile, foundationPile(i), 1))
      case from :: to :: number if from.startsWith("t") && to.startsWith("f") =>
        for(
          fromIndex <- getPileIndex(from);
          toIndex <- getPileIndex(to)
        ) yield(Move(tableauPile(fromIndex), foundationPile(toIndex),1))
      case from :: to :: number if from.startsWith("t") && to.startsWith("t") =>
        for(
          fromIndex <- getPileIndex(from);
          toIndex <- getPileIndex(to);
          numberOfCards <- toInt(number.mkString)
        ) yield(Move(tableauPile(fromIndex), tableauPile(toIndex),numberOfCards))   
      case from :: to :: number if from.startsWith("f") && to.startsWith("t") => 
        for(
          fromIndex <- getPileIndex(from);
          toIndex <- getPileIndex(to)
        ) yield(Move(foundationPile(fromIndex), tableauPile(toIndex),1))          
      case _ => None
    }
  }

  def moveToString(move: Move): String = 
    move.from.shortDescription + " " + move.to.shortDescription + " " + move.numberOfCards

  def playGame(game: Game): Unit = {
    game.renderBoard
    println(game.board.legalMoves.map(m => moveToString(m)))

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
