package controllers

import play.api.libs.json.Format.GenericFormat
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import repositories.Tables.{Users, UsersRow}
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.MySQLProfile.api._
import slick.lifted.TableQuery

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

class UserController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {


  implicit val usersFormat: OFormat[UsersRow] = Json.format[UsersRow]
  val users = TableQuery[Users]
  val db = Database.forConfig("mydb")

  def get(): Action[AnyContent] = Action.async {
    implicit request => {
      println(request.session.data)
      val id = request.session.get("id").get.toInt
      val res = db.run(users.filter(_.id === id).result)
      res.map { items =>
        Ok(Json.toJson(items))
      }
    }
  }
}
