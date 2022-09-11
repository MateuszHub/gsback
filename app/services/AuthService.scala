package services

import repositories.Tables.{Users, UsersRow}
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.MySQLProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

class AuthService {
  val users = TableQuery[Users]
  val db = Database.forConfig("mydb")

  def isAdmin(id: Int): Future[Boolean] = {
    val q = users.filter(_.id === id)
    val res = db.run(q.result)
    res.map(item => {
      if(item.nonEmpty) {
        item.head.isAdmin.get
      } else {
        false
      }
    })
  }

  def isRegistered(email: String): Future[Boolean] = {
    val q = users.filter(_.email === email)
    val res = db.run(q.result)
    res.map(item => {
      item.nonEmpty
    })
  }

  def getId(email: String): Future[Option[Int]] = {
    val q = users.filter(_.email === email)
    val res = db.run(q.result)
    res.map(item => {
      if (item.isEmpty)  Option.empty
      else Some(item.head.id)
    })
  }
  def register(email: String, name: String): Future[Int] = {
    val insertPlayerQuery = users += UsersRow(0, Some(email), Some(name))
    val insertResult: Future[Int] = db.run(insertPlayerQuery)
    insertResult
  }

}
