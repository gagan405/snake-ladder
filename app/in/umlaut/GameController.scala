package in.umlaut

import in.umlaut.entities._
import javax.inject.{Inject, Singleton}
import play.api.mvc.{AbstractController, ControllerComponents}

import scala.collection.mutable.Map

@Singleton
class GameController @Inject()(cc: ControllerComponents) extends AbstractController(cc){

    var games = Map[String, Game]()

    def roll(id: String) = Action {
        val game = games.get(id)
        if(game.isEmpty) {
            Ok("Game doesn't exist. Possible that it is already over?")
        } else {
            val status = games(id).roll()
            if (status.isFinished) {
                games.remove(id)
            }
            Ok(status.gameUpdates)
        }
    }

    def startGame = Action { request =>
        val json = request.body.asJson.get
        val players = json.as[Players]
        val board = Board.STANDARD_BOARD
        val game = new Game(board, players.players)
        games += (game.gameId.toString -> game)
        val sb = StringBuilder.newBuilder
        sb.append("Game started with players ")
          .append(players.players.map(p => p.name).mkString(" and "))
          .append('\n')
            .append("Game ID: ").append(game.gameId.toString)
            .append('\n')
            .append(Board.printBoard(board))
        Ok(sb.toString)
    }
}
