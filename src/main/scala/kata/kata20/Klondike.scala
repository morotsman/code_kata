package kata.kata20

import Game._

case class Position()

case class Move(val from: Position,val to: Position)

case class Board()

case class Game(val run: Turn) {

}

object Game {
  type Turn = Board => (List[Move], Option[Board])

  def takeTurn(move: Move): Game = Game(
    board => {
      val availableMoves = legalMoves().run(board)
      if(availableMoves._1.contains(move)) {
        val updatedBoard = ???
        (List(), Some(updatedBoard))
      } else {
        (List(), None)
      }
      
    })

  def legalMoves(): Game = Game(
    board => {
      (List(),Some(board))
    }    
  )

}

object Klondike {
  
  def playGame(board: Board) : Unit = {
    val (legalMoves,_) = Game.legalMoves().run(board)
    println(legalMoves)
    
    //get move from user
    val moveFromUser = ???
    
    val (positions1, newBoard) = Game.takeTurn(moveFromUser).run(board)
    
    if(newBoard == None) {
      println("Wrong move")
      playGame(board)
    }
    
    playGame(newBoard.get)
    
  }

  def main(args: Array[String]) {
    val initialBoard = Board()

    playGame(initialBoard)

  }

}