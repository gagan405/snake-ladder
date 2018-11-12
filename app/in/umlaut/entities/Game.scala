package in.umlaut.entities

import java.util.UUID

import scala.collection.mutable.ListBuffer
import scala.util.Random

class Game(val board: Board, players: List[Player]) {

    val gameId: UUID = UUID.randomUUID()
    val playBoard = board.getBoard

    val indexedPlayers = (players.indices zip players).toMap
    val firstPlayer = Random.nextInt(players.size)

    println("Randomly selected first player to roll the dice : " + indexedPlayers.get(firstPlayer).get.name)

    // current player keeps changing, so a var
    var currentPlayer = firstPlayer
    val playerMoves = indexedPlayers.keySet.map(k => (k, ListBuffer[Int](1))).toMap

    def handleAnyAnomalyAtNewPos(newPos: Int) = {
        val anyComponents = board.getComponentsAtPos(newPos)
        if(anyComponents.isDefined && anyComponents.get.head.getStartPos == newPos) {
            anyComponents.get.head.getEndPos
        } else newPos
    }

    def roll(): String = {
        val sb = StringBuilder.newBuilder
        val player = indexedPlayers.get(currentPlayer).get

        sb.append("Rolling for player ").append(currentPlayer).append('\n')

        val diceRead = 1 + Random.nextInt(6)
        sb.append("Dice reads ").append(diceRead).append('\n')

        val oldPos = playerMoves(currentPlayer).last
        val newPos = oldPos + diceRead

        if(newPos > playBoard.getMaxIdx) {
            sb.append("Tough luck. Player stays as it is.").append('\n')
            return sb.toString()
        } else if(newPos == playBoard.getMaxIdx) {
            return sb.append("Player ").append(player.name).append(" wins!!").append('\n')
              .append(Board.printBoard(playBoard)).toString()
        }

        sb.append("Player moves to ").append(newPos).append('\n')

        var moves = playerMoves(currentPlayer)
        moves += newPos

        val finalPos = handleAnyAnomalyAtNewPos(newPos)

        if (finalPos != newPos) {
            sb.append("Player encountered snake/ladder. Going further to ").append(finalPos).append('\n')
            moves += finalPos
        }

        setPlayerInPosition(player, finalPos)
        clearPlayerFromPosition(player, oldPos)

        sb.append("Player ")
          .append(player.name)
          .append(" moves: ")
          .append(playerMoves(currentPlayer).mkString("-->"))
          .append('\n')

        sb.append(Board.printBoard(playBoard))
        updatePlayer
        sb.toString()
    }

    /**
      *The below isnt really object-oriented!
      */
    private def setPlayerInPosition(player: Player, newPos: Int) = {
        val (row, col) = board.getRowColFromIdx(newPos)
        playBoard.board(row)(col) = playBoard.board(row)(col) + '\n' + player.character + currentPlayer + " " + player.name
    }

    private def clearPlayerFromPosition(player: Player, oldPos: Int) = {
        val (row, col) = board.getRowColFromIdx(oldPos)
        val playerString = "\n" + player.character + currentPlayer + " " + player.name
        playBoard.board(row)(col) = playBoard.board(row)(col).replace(playerString, "")
    }

    private def updatePlayer = {
        currentPlayer = if (currentPlayer == players.size - 1) 0 else currentPlayer + 1
    }
}
