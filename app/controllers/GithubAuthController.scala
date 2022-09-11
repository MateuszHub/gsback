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


case class GithubTokenResponse(accessToken: String, tokenType: String)

case class GithubInfoResp(email: String)
class GithubAuthController @Inject()(val controllerComponents: ControllerComponents)
                                    (configuration: Configuration)
  extends BaseController {

  val id = configuration.get[String]("github.appId")
  val key = configuration.get[String]("github.appKey")
  val url = "https://github.com/login/oauth/authorize"
  val tokenUrl = "https://github.com/login/oauth/access_token"
  val infoUrl = "https://api.github.com/user/emails"
  val redirect = configuration.get[String]("backend.host") + "/login/github/token"
  val redirectAfterToken = configuration.get[String]("frontend.host")
  val scope1 = "read:user"
  val scope2 = "user:email"


  def getLink(): Action[AnyContent] = Action {
    implicit request => {
      val redUrl = s"$url?client_id=$id&response_type=code&scope=$scope1 $scope2&redirect_uri=$redirect"
      Redirect(redUrl)
    }
  }

  def setToken(code: String): Action[AnyContent] = Action.async {
    implicit req => {
      println("Github " + code)
      implicit val githubTokenFormat = (
        (__ \ "access_token").read[String] and
          (__ \ "token_type").read[String]
        ) (GithubTokenResponse)
      implicit val uirFromat: OFormat[UserInfoResp] = Json.format[UserInfoResp]
      val request = basicRequest
        .body(Map("code" -> code,
          "client_id" -> id,
          "client_secret" -> key,
          "redirect_uri" -> redirect,
          "grant_type" -> "authorization_code"))
        .header("Accept", "application/json")
        .post(uri"$tokenUrl")

      val backend = HttpClientSyncBackend()
      val response: Response[Either[String, String]] = request.send(backend)

      response.body match {
        case Right(r) => {
          val tokenResponse: GithubTokenResponse = Json.parse(r).as[GithubTokenResponse]
          println(tokenResponse)
          val info = getInfo(tokenResponse.accessToken)
          val auth = new AuthService
          println(info)
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
                auth.register(info.email, info.email.split("@").head).flatMap { _ =>
                  auth.getId(info.email).flatMap(result => {
                    val id = result
                    if (id.isEmpty) {
                      Future(Ok("Something went wrong").withNewSession)
                    } else {
                      Future(Redirect(redirectAfterToken).withNewSession.addingToSession( "email" -> info.email, "id" -> id.get.toString))
                    }
                  })
                }
              };
            }
          }
        }
        case Left(value) => {
          println(value)
          Future(Ok("oh no"))
        }
        case _ => Future(Ok("empty"))
      }

    }
  }

  def getInfo(token: String): GithubInfoResp = {
    implicit val uirFromat = Json.format[GithubInfoResp]
    val request = basicRequest
      .header("Authorization", s"Bearer $token")
      .get(uri"$infoUrl")

    val backend = HttpClientSyncBackend()
    val response: Response[Either[String, String]] = request.send(backend)
    response.body match {
      case Right(r) => {
        println(r)
        val userInfoResp: Array[GithubInfoResp] = Json.parse(r).as[Array[GithubInfoResp]]
        userInfoResp.head;
      }
      case _ => {
        null;
      }
    }
  }
}
