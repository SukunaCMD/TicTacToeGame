import scala.util.Random

object Engine {
  case class Point(x: Int, y: Int)
  case class Player(mark: Mark) {
    def nextTurn: Player = this match {
      case Player(O) => Player(X)
      case Player(X) => Player(O)
    }
  }

  case class Game(board: TacBoard, winningPatterns: WinningPatterns, boardSize: Int,
                  playerO: IndexedSeq[Point], playerX: IndexedSeq[Point], curTurn: Player) {

    def addMove(point: Point): Game = curTurn match {
      case Player(O) => this.copy(board = board.update(point, curTurn.mark),
          playerO = playerO :+ point, curTurn = curTurn.nextTurn)

      case Player(X) => this.copy(board = board.update(point, curTurn.mark),
          playerX = playerX :+ point, curTurn = curTurn.nextTurn)
    }

    def curPlayer: IndexedSeq[Point] = curTurn match {
      case Player(O) => playerO
      case Player(X) => playerX
    }
    def checkWin: Boolean = {
      val winningSequences = winningPatterns.winList
      for(subset <- winningSequences)
        if(subset.intersect(curPlayer)==subset) return true

      false
    }
  }

  case class WinningPatterns(size: Int) {
    def winList: IndexedSeq[IndexedSeq[Point]] = {
      val diagonal1 = for {
        rowCol <- 1 to size
      } yield Point(rowCol, rowCol)
      val diagonal2 = for {
        x <- 1 to size
        y <- size to 1
      } yield Point(x, y)

      createVerticals ++ createHorizontals ++ IndexedSeq(diagonal2++diagonal1)
    }
    val createVerticals: IndexedSeq[IndexedSeq[Point]] = {
      for {
        curCol <- 1 to size
        vertWin = for(curColRow <- 1 to size) yield Point(curColRow, curCol)
      } yield vertWin
    }

    val createHorizontals: IndexedSeq[IndexedSeq[Point]] = {
      for {
        curRow <- 1 to size
        horizontalWin = for(curRowCOL <- 1 to size) yield Point(curRow, curRowCOL)
      } yield horizontalWin
    }

    def printWinningPatterns: Unit = {
      for(pattern <- winList) {
        println(s"Current pattern is: $pattern")
        for(point <- pattern) {
          println(s"Current point: $point")
        }
        println()
      }
    }
  }

  type Cells = Map[Point, Mark]

  trait Mark
  case object Empty extends Mark
  case object O extends Mark
  case object X extends Mark

  case class TacBoard(cells: Cells) {
    def update(point: Point, mark: Mark): TacBoard =
      this.copy(cells = cells + (point -> mark))
  }

  object Board {
    def initCells(size: Int): Cells = {
      val listPoints = for {
        x <- List.range(1, size + 1)
        y <- List.range(1, size + 1)
      } yield Point(x, y)

      listPoints.foldRight(Map[Point, Mark]()) { (a, b) =>
        b + (a -> Empty)
      }
    }

    def printBoard(board: Cells): Unit = {
      val size = Math.sqrt(board.size).toInt
      val seps = "-" * 8 * size

      println(seps)

      for (row <- 1 to size) {
        for (col <- 1 to size) {
          val curCell = board(Point(row, col))

          printf("| %4s ", curCell)
        }
        println("|")
        println(seps)
      }

    }
  }
}