import Engine.Board._
import Engine._

import scala.io.StdIn.{readInt, readLine}


object UI  {
  // for now the AI should just select randomly from point 1,1 to n,n based on available unfilled pos.
  //
  def emptyPos(game: Game, pos: Point): Input = {
    val board = game.board.cells
    if(board(pos)!=Empty) Reset
    else Continue
  }

  def addMove(point: Point, game: Game): (Game, Input) = {
    val newGame = game.addMove(point)
    newGame.checkWin match {
      case true => (newGame, Victory)
      case false => (newGame, Continue)
    }
  }

  def gameLoop(game: Game): Unit = {
    printBoard(game.board.cells)
    val move = boardPos
    emptyPos(game, move) match {
      case Reset => println("Unempty pos. Try new pos."); gameLoop(game)
      case Continue => println("Filling.")
    }
    val (newGame, inp) = addMove(move, game)
    inp match{
      case Victory => println("You win man gj gl"); System.exit(0)
      case Continue => println("The game will continue.")
    }
    gameLoop(newGame)
  }

  def boardPos: Point = {
    println("Where would you like to place your Mark?")
    println("row: ")
    val row = readInt()
    println("col: ")
    val col = readInt()
    Point(row, col)
  }


  sealed trait Input
  case object Reset extends Input
  case object Continue extends Input
  case object Victory extends Input
}
