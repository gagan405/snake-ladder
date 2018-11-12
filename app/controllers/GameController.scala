package controllers

import in.umlaut.entities._
import javax.inject.{Inject, Singleton}
import play.api.mvc.{AbstractController, ControllerComponents}
import scala.collection.mutable.Map


@Singleton
class GameController @Inject()(cc: ControllerComponents) extends AbstractController(cc){

    var games = Map[String, Game]()

    def roll(id: String) = Action { Ok(games(id).roll()) }

    def startGame = Action {
        val board = Board.STANDARD_BOARD
        val players = List(Player("Hagga"), Player("Tatti"))
        val game = new Game(board, players)
        games += (game.gameId.toString -> game)
        val sb = StringBuilder.newBuilder
        sb.append("Game started with players Hagga and Tatti").append('\n')
            .append("Game ID: ").append(game.gameId.toString)
            .append('\n')
            .append(Board.printBoard(board))
        Ok(sb.toString)
    }

    def printBoard = Action {
        Ok("")
    }
}
