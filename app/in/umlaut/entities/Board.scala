package in.umlaut.entities

case class Cell(number: Int, boardComponents: List[BoardComponent] = List())

class Board(val height: Int,
            val length: Int,
            val ladders: List[Ladder],
            val snakes: List[Snake]) {

  val board = getBoardMatrix

  ladders.zipWithIndex.foreach(x => setEntity(x._1, x._2 + 1))
  snakes.zipWithIndex.foreach(x => setEntity(x._1, x._2 + 1))

  def getBoard = new Board(height, length, ladders, snakes)

  def getRowColFromIdx(startPos: Int) = {
      val row = (startPos -1)/ length
      if (row % 2 == 1) {
          (row, length - (startPos - length * row))
      } else {
          (row, startPos - length * row - 1)
      }
  }

  private def setEntity(entity: BoardComponent, idx: Int) = {
      require(entity.getEndPos <= height * length)
      require(entity.getStartPos <= height * length)
      require(entity.getStartPos > 0)
      require(entity.getStartPos > 0)

      val (row, col) = getRowColFromIdx(entity.getStartPos)
      board(row)(col) = board(row)(col) + '\n' + entity.character + idx + 's'
      val (rowe, cole) = getRowColFromIdx(entity.getEndPos)
      board(rowe)(cole) = board(rowe)(cole) + '\n' + entity.character + idx + 'e'
  }

  override def toString = {
      Board.printBoard(this)
  }

  private def getBoardMatrix = {
    val init = Array.tabulate(height,length)((i, j) => (length * i) + (j + 1) + "")
    val board = init.zipWithIndex.map(x => {
      if(x._2 % 2 != 0) x._1.reverse
      else  x._1
    })
    board
  }
}

object Board {

  val STANDARD_BOARD = standardBoard

  private def standardBoard = {
    val ladders = List(Ladder(5,29), Ladder(13, 54), Ladder(31, 98))
    val snakes = List(Snake(45, 2), Snake(97, 47))
    new Board(10, 10, ladders, snakes)
  }

  def printBoard(board: Board) = {
    val gameBoard = board.board
    def drawRowPartition(length: Int, colLengths: Map[Int,Int]) = {
      def drawSingleCellRoof(col: Int, size: Int) = {
        if(col == 0) {
          "+" + "-" * size + "+"
        } else {
          "-" * size + "+"
        }
      }
      (0 until length).map(i => drawSingleCellRoof(i, colLengths(i))).mkString + '\n'
    }

    def printRow(row: Array[String], colLengths: Map[Int,Int], rowIdx: Int) = {
      def getColText(str: String, margin: Int): Any = {
        require(margin >= str.length)
        str + " " * (margin - str.length - 2)
      }
      val subRows = row.map(_.split('\n'))
      val maxHeight = subRows.map(_.length).max
      (0 until maxHeight).map(i => {
        "| " +
          subRows.zipWithIndex.map(sr => {
            val rightMargin = if (sr._2 == board.length -1) " |" else " | "
            getColText(sr._1.lift(i).getOrElse(""), colLengths(sr._2)) + rightMargin
          }).mkString + '\n'
      }).mkString
    }

    require(board.height > 0 && board.length > 0)

    val colLengths = gameBoard.transpose
      .map(a => a.map(_.split('\n').map(_.length).max + 2).max)
      .zipWithIndex.map(x => (x._2, x._1))
      .toMap

    val sb = StringBuilder.newBuilder
    sb.append(drawRowPartition(board.length, colLengths))
    gameBoard.reverse.zipWithIndex.foreach {
      case(row, i) => {
        sb.append(printRow(row, colLengths, i))
        sb.append(drawRowPartition(board.length, colLengths))
      }
    }
    sb.mkString
  }

}
