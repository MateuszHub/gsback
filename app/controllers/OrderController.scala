package controllers

import com.stripe.model.Product
import com.stripe.Stripe
import com.stripe.model.checkout.Session
import com.stripe.param.checkout.SessionCreateParams.LineItem.PriceData
import play.api.libs.json.Format.GenericFormat
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import repositories.Tables.{Items, ItemsOrders, ItemsOrdersRow, ItemsRow, Keys, Orders, OrdersRow}
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.MySQLProfile.api._
import slick.lifted.TableQuery

import java.sql.Date
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import com.stripe.param.checkout.SessionCreateParams
import com.stripe.net.Webhook
import play.api.Configuration

import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsJava

class OrderController @Inject()(val controllerComponents: ControllerComponents)
                               (configuration: Configuration)
  extends BaseController {

  implicit val orderFormat: OFormat[OrdersRow] = Json.format[OrdersRow]
  val keys = TableQuery[Keys]
  val orders = TableQuery[Orders]
  val items = TableQuery[Items]
  val itemsOrders = TableQuery[ItemsOrders]
  val db = Database.forConfig("mydb")

  def afterPay() = Action(parse.raw).async {
    req => {
      val sigHeader = req.headers.get("Stripe-Signature").get
      val payload = req.body.asBytes().map(_.utf8String).getOrElse("")
      val event = Webhook.constructEvent(payload, sigHeader, configuration.get[String]("stripe.appSecret"))
      if (event.getType == "checkout.session.completed") {
        val sess = event.getDataObjectDeserializer().getObject().get().asInstanceOf[Session];
        if (sess.getPaymentStatus == "paid") {
          val orderq = orders.filter(_.payid === sess.getId)
          val orderr = db.run(orderq.result)
          orderr.map { order =>
            if (order.nonEmpty) {
              val newItem = order.head.copy(paid = Some(sess.getAmountTotal.toInt))
              val q = orderq.update(newItem)
              db.run(q)
              (Ok("paid"))
            } else {
              (NotFound("Not found order " + sess.getId))
            }
          }
        } else {
          Future(NotFound("Wrong event type"))
        }
      } else {
        Future(NotFound("Wrong event type"))
      }

    }
  }

  def pay(id: Int) = Action.async {
    Stripe.apiKey = configuration.get[String]("stripe.appSecret")
    val domain = configuration.get[String]("backend.host") + s"/orders/${id}/afterpay"

    val orderq = orders.filter(_.id === id)
    val orderr = db.run(orderq.result)
    orderr.flatMap { order =>
      if (order.nonEmpty) {
        val ioq = itemsOrders.filter(_.orderId === id)
        val params = SessionCreateParams.builder
          .setMode(SessionCreateParams.Mode.PAYMENT)
          .setSuccessUrl(domain + "?success=true")
          .setCancelUrl(domain + "?canceled=true");
        db.run(ioq.result).flatMap {
          itemsInOrder => {
            Future.sequence(itemsInOrder.map {
              singleItem => {
                val itemQ = items.filter(_.id === singleItem.itemId)
                db.run(itemQ.result).map(seq => seq.head)
              }
            })
          }
        }.map {
          items => {
            println(items)
            items.map { item =>
              val priceData = productToPriceData(item)
              params.addLineItem(SessionCreateParams.LineItem.builder.setQuantity(1L).setPriceData(priceData).build)
            }
            params
          }
        }.flatMap { seq =>
          val session = Session.create(params.build())
          val newItem = order.head.copy(link = Some(session.getUrl), payid = Some(session.getId))
          val q = orderq.update(newItem)
          db.run(q)
          Future(Ok(session.getUrl))
        }
      } else
        Future(NotFound("Not found"))
    }
  }

  def productToPriceData(item: ItemsRow): PriceData = {
    var param: mutable.HashMap[String, Object] = mutable.HashMap()
    param += ("name" -> item.name.get)
    val product = Product.create(param.asJava)
    new PriceData.Builder().setProduct(product.getId)
      .setCurrency("PLN")
      .setUnitAmount(item.price.get)
      .setTaxBehavior(PriceData.TaxBehavior.INCLUSIVE)
      .build();
  }

  def getAll(): Action[AnyContent] = Action.async {
    request => {
      val id = request.session.get("id").get.toInt
      val res = db.run(orders.filter(_.userId === id).result)
      res.map { items =>
        Ok(Json.toJson(items))
      }
    }
  }

  def add(): Action[AnyContent] = Action.async {
    request => {
      val id = request.session.get("id").get
      val content = request.body
      val jsonObject = content.asJson
      val order: Option[List[Int]] =
        jsonObject.flatMap(
          Json.fromJson[List[Int]](_).asOpt
        )
      val o = order.get
      val price = new AtomicInteger(0);
      val itemsInCart = Future.sequence(o.map(id => {
        val singleItem = db.run(items.filter(_.id === id).result)
        singleItem.map(si => {
          if (si.nonEmpty) {
            price.addAndGet(si.head.price.getOrElse(0))
          }
          si.head
        })
      }))
      itemsInCart.flatMap(iIC => {
        val newOrder = OrdersRow(
          id = 0,
          userId = Some(id.toInt),
          when = Some(new Date(Instant.now().toEpochMilli)),
          total = Some(price.get()))
        val insertQ = (orders returning orders.map(_.id)) += newOrder
        val insertResult: Future[Int] = db.run(insertQ)
        insertResult.map { insertedId =>
          iIC.foreach(siic => {
            val newItemOrder = ItemsOrdersRow(0, Some(siic.id), Some(insertedId))
            assignKeyToOrder(insertedId, siic.id)
              .map(_ => {
                val insertIOQ = itemsOrders += newItemOrder
                db.run(insertIOQ)
              })
          })
          Ok(Json.toJson(insertedId))
        }
      })
    }
  }

  def assignKeyToOrder(orderId: Int, itemId: Int): Future[Boolean] = {
    val q = (for {
      item <- {
        val byItemId = keys.filter(_.itemId === itemId).filter(_.orderId.isEmpty).forUpdate;
        byItemId.result
      }
      _ <- {
        val newKey = item.head.copy(orderId = Some(orderId))
        keys.filter(_.id === newKey.id).update(newKey)
      }
    } yield ()).transactionally
    db.run(q).map(_ => true)
  }


}