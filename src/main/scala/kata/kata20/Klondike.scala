package kata.kata20

import GameEngine.Turn
import KlondikeBoard.Pile

case class Position()

case class Result()

case class Move(val from: Position,val to: Position)

case class Card(val suite: Suite.Value, val value: Int, val hidden: Boolean)

object Suite extends Enumeration {
  val Hearts, Spades, Diamonds,Clubs = Value
}

trait Board


case class Game(val rules: Rules, val board: Board) {
  def legalMoves(): List[Move] = 
    rules.legalMoves(this)
}

trait Rules {
  def legalMoves(game: Game): List[Move];
}

case class KlondikeBoard(val foundationPile1: Pile, val foundationPile2: Pile, val foundationPile3: Pile, val foundationPile4: Pile,
    val tableauPile1: Pile, val tableauPile2: Pile, val tableauPile3: Pile, val tableauPile4: Pile,
    val tableauPile5: Pile, val tableauPile6: Pile, val tableauPile7: Pile, val discardPile: Pile, val stockPile: Pile) extends Board {
   
}

case object KlondikeBoard {
  type Pile = List[Card]
}

object KlondikeRules extends Rules {
  
  def legalMoves(game: Game): List[Move] = ???
  
}

case class GameEngine(val run: Turn)

object GameEngine {
  type Turn = Game => (Result, Option[Game])

  def takeTurn(move: Move): GameEngine = GameEngine(
    (game:Game) => {
      val availableMoves = game.legalMoves()
      if(availableMoves.contains(move)) {
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
    (2 to 14).toList.map(v => Card(suite,v,true))
  
  def shuffelCards(cards: List[Card]) =
    scala.util.Random.shuffle(cards)
    
    
  def generate: KlondikeBoard = {
    val foundationPile1: Pile = List()
    val foundationPile2: Pile = List() 
    val foundationPile3: Pile = List()
    val foundationPile4: Pile = List()
    val randomCards = shuffelCards(cards)
    val tableauPile1: Pile = randomCards.take(1)
    val tableauPile2: Pile = randomCards.drop(1).take(2) 
    val tableauPile3: Pile = randomCards.drop(3).take(3)
    val tableauPile4: Pile = randomCards.drop(6).take(4)
    val tableauPile5: Pile = randomCards.drop(10).take(5)
    val tableauPile6: Pile = randomCards.drop(15).take(6)
    val tableauPile7: Pile = randomCards.drop(21).take(7)
    val discardPile: Pile = List()
    val stockPile: Pile = randomCards.drop(28)
    KlondikeBoard(foundationPile1, foundationPile2, foundationPile3, foundationPile4,
    tableauPile1, tableauPile2, tableauPile3, tableauPile4,
    tableauPile5, tableauPile6, tableauPile7, discardPile, stockPile)
  }
  
}

object Klondike {
  
  def playGame(game: Game) : Unit = {
    println(game.board)
    
    //get move from user
    val moveFromUser = ???
    
    val (result, newGame) = GameEngine.takeTurn(moveFromUser).run(game)
    
    if(newGame == None) {
      println("Wrong move")
      playGame(game)
    } else {
      playGame(newGame.get)
    }
    
    
    
  }

  def main(args: Array[String]) {
    val game = Game(KlondikeRules, KlondikeBoardGenerator.generate)

    playGame(game)

  }

}