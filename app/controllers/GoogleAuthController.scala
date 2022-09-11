package controllers

import play.api.Configuration
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import services.AuthService
import sttp.client3.{HttpClientSyncBackend, Response, UriContext, basicRequest}

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class TokenResponse(accessToken: String, expiresIn: Int)

case class UserInfoResp(name: String, id: String, email: String)

class GoogleAuthController @Inject()(val controllerComponents: ControllerComponents)
                                    (configuration: Configuration)
  extends BaseController {

  val id = configuration.get[String]("google.appId")
  val key = configuration.get[String]("google.appKey")
  val url = "https://accounts.google.com/o/oauth2/v2/auth"
  val tokenUrl = "https://oauth2.googleapis.com/token"
  val infoUrl = "https://www.googleapis.com/oauth2/v2/userinfo"
  val redirect = configuration.get[String]("backend.host") + "/login/google/token"
  val redirectAfterToken = configuration.get[String]("frontend.host")
  val scope1 = "https://www.googleapis.com/auth/userinfo.profile"
  val scope2 = "https://www.googleapis.com/auth/userinfo.email"


  def getLink(): Action[AnyContent] = Action {
    implicit request => {
      val redUrl = s"$url?client_id=$id&response_type=code&scope=$scope1 $scope2&redirect_uri=$redirect"
      Redirect(redUrl)
    }
  }

  def setToken(code: String): Action[AnyContent] = Action.async {
    implicit req => {
      implicit val tokenFormat = (
        (__ \ "access_token").read[String] and
          (__ \ "expires_in").read[Int]
        ) (TokenResponse)
      implicit val uirFromat: OFormat[UserInfoResp] = Json.format[UserInfoResp]
      val request = basicRequest
        .body(Map("code" -> code,
          "client_id" -> id,
          "client_secret" -> key,
          "redirect_uri" -> redirect,
          "grant_type" -> "authorization_code"))
        .post(uri"$tokenUrl")

      val backend = HttpClientSyncBackend()
      val response: Response[Either[String, String]] = request.send(backend)

      response.body match {
        case Right(r) => {
          val tokenResponse: TokenResponse = Json.parse(r).as[TokenResponse]
          println(tokenResponse)
          val info = getInfo(tokenResponse.accessToken)
          val auth = new AuthService
          auth.isRegistered(info.email).flatMap {
            value => {
              println("Value is in db? = " + value)
              if (value) {
                auth.getId(info.email).flatMap(result => {
                  val id = result
                  if (id.isEmpty) {
                    Future(Ok("Something went wrong").withNewSession)
                  } else {
                    Future(Redirect(redirectAfterToken).withNewSession.addingToSession("email" -> info.email, "id" -> id.get.toString))
                  }
                })
              } else {
                auth.register(info.email, info.name).flatMap { _ =>
                  auth.getId(info.email).flatMap(result => {
                    val id = result
                    if (id.isEmpty) {
                      Future(Ok("Something went wrong").withNewSession)
                    } else {
                      Future(Redirect(redirectAfterToken).withNewSession.addingToSession("email" -> info.email, "id" -> id.get.toString))
                    }
                  })
                }
              };
            }
          }
        }
        case _ => Future(Ok("empty"))
      }

    }
  }

  def getInfo(token: String): UserInfoResp = {
    implicit val uirFromat = Json.format[UserInfoResp]
    val request = basicRequest
      .header("Authorization", s"Bearer $token")
      .get(uri"$infoUrl")

    val backend = HttpClientSyncBackend()
    val response: Response[Either[String, String]] = request.send(backend)
    response.body match {
      case Right(r) => {
        println(r)
        val userInfoResp: UserInfoResp = Json.parse(r).as[UserInfoResp]
        userInfoResp;
      }
      case _ => {
        null;
      }
    }
  }
}
