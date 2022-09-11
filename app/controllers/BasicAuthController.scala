package controllers

import at.favre.lib.crypto.bcrypt.BCrypt
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import play.filters.csrf.CSRF
import repositories.Tables.{Logins, LoginsRow}
import services.AuthService
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.MySQLProfile.api._
import slick.lifted.TableQuery

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps


class BasicAuthController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {

  implicit val usersFormat: OFormat[LoginsRow] = Json.format[LoginsRow]
  val logins = TableQuery[Logins]
  val db = Database.forConfig("mydb")

  def login(): Action[AnyContent] = Action.async {
    implicit request => {
      val content = request.body
      val auth = new AuthService
      val data = content.asFormUrlEncoded
      val email = data.get("email").head
      val p = data.get("password").head
      val bc = BCrypt.verifyer();
      val res = db.run(logins.filter(_.email === email).result)
      res.flatMap { login =>
        if (login.nonEmpty) {
          if (bc.verify(p.toCharArray, login.head.password.get.toCharArray).verified) {
            auth.getId(email).map(result => {
              val id = result
              if (id.isEmpty) {
                (Unauthorized("Something went wrong").withNewSession)
              } else {
                (Ok(email).addingToSession("email" -> email, "id" -> id.get.toString))
              }
            })
          } else {
            Future(Unauthorized("Wrong cred").withNewSession)
          }
        } else {
          Future(Unauthorized("Not exists").withNewSession)
        }
      }
    }
  }

  def register(): Action[AnyContent] = Action.async {
    request => {
      val content = request.body
      val auth = new AuthService
      val data = content.asJson
      val email = Json.fromJson[String](data.get("email")).get
      val p = Json.fromJson[String](data.get("password")).get
      val bc = BCrypt.withDefaults();
      val res = db.run(logins.filter(_.email === email).result)

      res.flatMap { login =>
        if (login.isEmpty) {
          val hash = bc.hashToString(15, p.toCharArray)
          auth.getId(email).flatMap(result => {
            val id = result
            if (id.isEmpty) {
              auth.register(email, email.split("@").head)
              val newLogin = LoginsRow(id = 0, email = Some(email), password = Some(hash.value))
              val insertQ = logins += newLogin
              val insertResult: Future[Int] = db.run(insertQ)
              insertResult.map(_ =>
                (Ok("Ok").withNewSession))
            } else {
              Future(Unauthorized("Already registered").withNewSession)
            }
          })
        } else {
          Future(Unauthorized("Wrong").withNewSession)
        }
      }
    }
  }

  def logout() =  Action {
    Ok.withNewSession
  }
}
