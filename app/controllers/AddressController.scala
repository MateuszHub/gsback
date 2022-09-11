package controllers

import play.api.libs.json.Format.GenericFormat
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import repositories.Tables.{Adresses, AdressesRow}
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.MySQLProfile.api._
import slick.lifted.TableQuery

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

class AddressController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  implicit val addressFormat: OFormat[AdressesRow] = Json.format[AdressesRow]
  val addresses = TableQuery[Adresses]
  val db = Database.forConfig("mydb")

  def getAll(): Action[AnyContent] = Action.async {
    request => {
      val id = request.session.get("id").get.toInt
      println(id)
      val res = db.run(addresses.filter(_.userId === id).result)
      res.map { items =>
        Ok(Json.toJson(items))
      }
    }
  }

  def add(): Action[AnyContent] = Action.async {
    request => {
      val id = request.session.get("id").get
      val content = request.body
      println(content)
      val jsonObject = content.asJson
      val address: Option[AdressesRow] =
        jsonObject.flatMap(
          Json.fromJson[AdressesRow](_).asOpt
        )
      val a = address.get
      val newAddr = a.copy(userId = Some(id.toInt))
      val insertQ = addresses += newAddr
      val insertResult: Future[Int] = db.run(insertQ)
      insertResult.map { inserted =>
        Ok(Json.toJson(a))
      }
    }
  }

}