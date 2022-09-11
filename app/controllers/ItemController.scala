package controllers

import play.api.libs.json.Format.GenericFormat
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import repositories.Tables.ItemsRow
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.MySQLProfile.api._
import slick.lifted.TableQuery

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

case class ItemsRowReturnDTO(id: Int, name: Option[String] = None, desc: Option[String] = None, price: Option[Int] = None, active: Option[Boolean] = Some(true), img: Option[String] = None, quantity: Option[Int] = Some(0))

class ItemController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {
  val items = TableQuery[repositories.Tables.Items]
  val keys = TableQuery[repositories.Tables.Keys]

  implicit val itemReturnFormat: OFormat[ItemsRowReturnDTO] = Json.format[ItemsRowReturnDTO]
  implicit val itemFormat: OFormat[ItemsRow] = Json.format[ItemsRow]

  val db = Database.forConfig("mydb")

  def getAll(): Action[AnyContent] = Action.async {
    val res = db.run(items.result)
    res.flatMap { _items =>
      Future.sequence(_items.map(singleItem => {
        db.run(keys.filter(_.itemId === singleItem.id).filter(_.orderId.isEmpty).result).map {
          foundKeys => foundKeys.size
        }.map(count => {
          ItemsRowReturnDTO(singleItem.id, singleItem.name, singleItem.desc, singleItem.price, singleItem.active, singleItem.img, Some(count))
        })
      })).map(list => Ok(Json.toJson(list)))
    }
  }

  def add(): Action[AnyContent] = Action.async {
    request =>
      val content = request.body
      val jsonObject = content.asJson
      val item: Option[ItemsRow] =
        jsonObject.flatMap(
          Json.fromJson[ItemsRow](_).asOpt
        )
      val insertPlayerQuery = items += item.get
      val insertResult: Future[Int] = db.run(insertPlayerQuery)
      insertResult.map {
        res => Ok(res.toString)
      }
  }

  def deactive(id: Int): Action[AnyContent] = Action.async {
    request =>
      val db = Database.forConfig("mydb")
      val itemById = items.filter(_.id === id)
      val res = db.run(itemById.result)
      res.map { item =>
        if (item.nonEmpty) {
          val newItem = item.head.copy(active = Some(false))
          val q = itemById.update(newItem)
          db.run(q)
          Ok(Json.toJson(newItem))
        } else
          Ok(Json.toJson("Not found"))
      }
  }
}
