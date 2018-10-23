package in.umlaut.entities

import java.util.UUID

import scala.collection.mutable.ListBuffer
import scala.util.Random

class Game(val board: Board, players: List[Player]) {

    val gameId: UUID = UUID.randomUUID()
    val playBoard = board.getBoard

    val indexedPlayers = (players.indices zip players).toMap
    val firstPlayer = Random.nextInt(players.size)

    var currentPlayer = firstPlayer
    val playerMoves = indexedPlayers.keySet.map(k => (k, ListBuffer[Int](1))).toMap

    def roll() = {
        val sb = StringBuilder.newBuilder
        val player = indexedPlayers.get(currentPlayer).get

        sb.append("Rolling for player ").append(currentPlayer).append('\n')

        val diceRead = 1 + Random.nextInt(6)
        sb.append("Dice reads ").append(diceRead).append('\n')

        val newPos = playerMoves(currentPlayer).last + diceRead
        var moves = playerMoves(currentPlayer)
        sb.append("Player moves to ").append(newPos).append('\n')
        moves += newPos

        val (row, col) = board.getRowColFromIdx(newPos)
        playBoard.board(row)(col) = playBoard.board(row)(col) + '\n' + player.character + currentPlayer + " " + player.name

        sb.append("Player ").append(player.name).append(" moves: ").append(playerMoves(currentPlayer).mkString("-->"))

        sb.append(Board.printBoard(playBoard))
        updatePlayer
        sb.toString()
    }

    private def updatePlayer = {
        currentPlayer = if (currentPlayer == players.size - 1) 0 else currentPlayer + 1
    }
}
