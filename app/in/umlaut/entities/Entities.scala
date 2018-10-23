package in.umlaut.entities


trait BasicBoardComponent {
    def character: Char
}

trait BoardComponent extends BasicBoardComponent {
    def getStartPos: Int
    def getEndPos: Int

    def getParticipatingCells = List(getStartPos, getEndPos)

    def isStart(value: Int) = getStartPos == value
    def isEnd(value: Int) = getEndPos == value
}

case class Ladder(startPos: Int, endPos: Int) extends BoardComponent {
    val character = 'L'
    require(startPos < endPos && startPos > 0)

    override def getStartPos: Int = startPos
    override def getEndPos: Int = endPos
}

case class Snake(startPos: Int, endPos: Int) extends BoardComponent {
    val character = 'S'
    require(startPos > endPos && endPos >= 0)

    override def getStartPos: Int = startPos
    override def getEndPos: Int = endPos
}

case class Player(name: String) extends BasicBoardComponent {
    val character = 'P'
}