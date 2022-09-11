package controllers

import play.api.libs.json.Format.GenericFormat
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import repositories.Tables._
import services.AuthService
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.MySQLProfile.api._
import slick.lifted.TableQuery

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

class KeysController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  implicit val keysFormat: OFormat[KeysRow] = Json.format[KeysRow]
  val keysTable = TableQuery[Keys]
  val orders = TableQuery[Orders]
  val db = Database.forConfig("mydb")


  def add(_itemId: Int): Action[AnyContent] = Action.async {
    implicit request => {
      val id = request.session.get("id").get.toInt

      val auth = new AuthService
      auth.isAdmin(id).flatMap(
        isAdmin => {
          if(!isAdmin) {
            Future(Unauthorized)
          } else {
            val content = request.body
            val jsonObject = content.asJson
            val keys: Option[List[String]] =
              jsonObject.flatMap(
                Json.fromJson[List[String]](_).asOpt
              )
            Future.sequence(keys.get.map(_key => {
              val newKey = KeysRow(
                id = 0,
                itemId = Some(_itemId),
                key = Some(_key))
              if(_key.trim.nonEmpty) {
                val insertQ = keysTable += newKey
                db.run(insertQ)
              } else {
                Future(None)
              }
            })).flatMap(_ => Future(Ok))
          }
        }
      )
    }
  }

  def get(orderId: Int): Action[AnyContent] = Action.async {
    request => {
      val id = request.session.get("id").get
      val order = db.run(orders.filter(_.userId === id.toInt).filter(_.id === orderId).result)
      order.flatMap { _order =>
        if (_order.isEmpty) {
          Future(Ok)
        } else {
          val res = db.run(keysTable.filter(_.orderId === _order.head.id).result)
          res.map { keys =>
            Ok(Json.toJson(keys))
          }
        }
      }

    }
  }

}