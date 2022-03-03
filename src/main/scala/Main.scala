import Engine.Board.initCells
import Engine._
import UI._

import scala.io.StdIn.readInt

object Main {
  def askBoardSize: Int = {
    println("What size board would you like to use?")
    val size = readInt
    size
  }

  def main(args: Array[String]): Unit = {
    val boardSize = askBoardSize
    val winningPatterns = WinningPatterns(boardSize)
    val board = TacBoard(initCells(boardSize))
    val game = Game(board, winningPatterns, boardSize, IndexedSeq(), IndexedSeq(), Player(O))
    gameLoop(game)
  }
}
