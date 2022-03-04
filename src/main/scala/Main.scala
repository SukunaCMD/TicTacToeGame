import Engine.Board.initCells
import Engine._
import UI._
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.io.StdIn.readInt

object Main {
  def askBoardSize: Int = {
    println("What size board would you like to use?")
    val size = readInt
    size
  }

  def createGame: IO[Game] = {
    for {
      _ <- IO {println("Hey what size board?")}
      size <- IO { readInt }
      board <- IO {TacBoard(initCells(size))}
      winningPatterns <- IO{WinningPatterns(size)}
      game <- IO { Game(board,winningPatterns, Player(O))}
    } yield game
  }

  def main(args: Array[String]): Unit = {
//    val boardSize = askBoardSize
//    val winningPatterns = WinningPatterns(boardSize)
//    val board = TacBoard(initCells(boardSize))
//    val game = Game(board, winningPatterns, Player(O))
    val game = createGame.unsafeRunSync()
    loop(game).unsafeRunSync()
  }
}
