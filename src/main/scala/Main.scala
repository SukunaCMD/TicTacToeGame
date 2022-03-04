import Engine._
import UI._
import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {

  def run: IO[Unit] = {
    IO.println("Board Size?") >> IO.readLine.flatMap{
        size: String =>
          if(size.forall(Character.isDigit)) {
            val board = Board.initCells(size.toInt)
            val patterns = WinningPatterns(size.toInt)
            val game = Game(TacBoard(board), patterns, Player(O))
            gameLoop(game)
          }
          else run
      }
    }
}

