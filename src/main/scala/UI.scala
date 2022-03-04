import Engine.Board._
import Engine._
import cats.effect.{ExitCode, IO}

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

  def gameLoop(game: Game): IO[Unit] = {
    IO.pure(printBoard(game.board))
    getPosInp.flatMap{
      pt: Point => emptyPos(game, pt) match {
        case Reset => gameLoop(game)
        case Continue => addMove(pt, game) match {
          case (_, Victory) => IO.println("Victor!!!").as(ExitCode(1))
          case (game, Continue) => gameLoop(game)
        }
      }
    }
  }

  def getPosInp: IO[Point] = {
    IO.println("Row, col?") >> IO.readLine.flatMap{
      row => isAllDigits(row) match {
        case true => IO.readLine.flatMap{
          col => isAllDigits(col) match {
            case true => IO.pure(Point(row.toInt, col.toInt))
            case false => getPosInp
          }
        }
        case false => getPosInp
      }
    }
  }




  def isAllDigits(s: String) = s.forall(Character.isDigit)

  sealed trait Input
  case object Reset extends Input
  case object Continue extends Input
  case object Victory extends Input
}
