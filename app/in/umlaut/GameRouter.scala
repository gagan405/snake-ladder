package in.umlaut

import javax.inject.{Inject, Singleton}
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

@Singleton
class GameRouter @Inject()(controller: GameController) extends SimpleRouter {

  override def routes: Routes = {
    case POST(p"/$id/roll") =>
      controller.roll(id)
    case POST(p"/start") =>
      controller.startGame
  }



}
