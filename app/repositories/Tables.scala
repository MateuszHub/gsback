package repositories
// AUTO-GENERATED Slick data model

/** Stand-alone Slick data model for immediate use */
object Tables extends {
  val profile = slick.jdbc.MySQLProfile
  val NOT_SUPPORTED_TEXT = "Inserting into ? projection not supported."
  import profile.api._
  import slick.model.ForeignKeyAction
  // NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.
  import slick.jdbc.{GetResult => GR}

  /** DDL for all tables. Call .create to execute. */
  lazy val schema: profile.SchemaDescription = Array(Adresses.schema, Items.schema, ItemsOrders.schema, Logins.schema, Orders.schema, Users.schema).reduceLeft(_ ++ _)

  @deprecated("Use .schema instead of .ddl", "3.0")
  def ddl = schema

  /** Entity class storing rows of table Adresses
   *
   * @param id      Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey
   * @param userId  Database column user_id SqlType(MEDIUMINT), Default(None)
   * @param line1   Database column line1 SqlType(TEXT), Default(None)
   * @param line2   Database column line2 SqlType(TEXT), Default(None)
   * @param city    Database column city SqlType(TEXT), Default(None)
   * @param country Database column country SqlType(TEXT), Default(None)
   * @param zip     Database column zip SqlType(TEXT), Default(None) */
  case class AdressesRow(id: Int, userId: Option[Int] = None, line1: Option[String] = None, line2: Option[String] = None, city: Option[String] = None, country: Option[String] = None, zip: Option[String] = None)

  /** Table description of table adresses. Objects of this class serve as prototypes for rows in queries. */
  class Adresses(_tableTag: Tag) extends profile.api.Table[AdressesRow](_tableTag, Some("gamestore"), "adresses") {
    def * = (id, userId, line1, line2, city, country, zip) <> (AdressesRow.tupled, AdressesRow.unapply)

    /** Maps whole row to an option. Useful for outer joins. */
    def ? = ((Rep.Some(id), userId, line1, line2, city, country, zip)).shaped.<>({ r => import r._; _1.map(_ => AdressesRow.tupled((_1.get, _2, _3, _4, _5, _6, _7))) }, (_: Any) => throw new Exception(NOT_SUPPORTED_TEXT))

    /** Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey */
    val id: Rep[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column user_id SqlType(MEDIUMINT), Default(None) */
    val userId: Rep[Option[Int]] = column[Option[Int]]("user_id", O.Default(None))
    /** Database column line1 SqlType(TEXT), Default(None) */
    val line1: Rep[Option[String]] = column[Option[String]]("line1", O.Default(None))
    /** Database column line2 SqlType(TEXT), Default(None) */
    val line2: Rep[Option[String]] = column[Option[String]]("line2", O.Default(None))
    /** Database column city SqlType(TEXT), Default(None) */
    val city: Rep[Option[String]] = column[Option[String]]("city", O.Default(None))
    /** Database column country SqlType(TEXT), Default(None) */
    val country: Rep[Option[String]] = column[Option[String]]("country", O.Default(None))
    /** Database column zip SqlType(TEXT), Default(None) */
    val zip: Rep[Option[String]] = column[Option[String]]("zip", O.Default(None))

    /** Foreign key referencing Users (database name adresses_ibfk_1) */
    lazy val usersFk = foreignKey("adresses_ibfk_1", userId, Users)(r => Rep.Some(r.id), onUpdate = ForeignKeyAction.NoAction, onDelete = ForeignKeyAction.NoAction)
  }

  /** Collection-like TableQuery object for table Adresses */
  lazy val Adresses = new TableQuery(tag => new Adresses(tag))

  /** Entity class storing rows of table Items
   *
   * @param id     Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey
   * @param name   Database column name SqlType(VARCHAR), Length(255,true), Default(None)
   * @param desc   Database column desc SqlType(TEXT), Default(None)
   * @param price  Database column price SqlType(MEDIUMINT), Default(None)
   * @param active Database column active SqlType(BIT), Default(Some(true))
   * @param img    Database column img SqlType(VARCHAR), Length(255,true), Default(None) */
  case class ItemsRow(id: Int, name: Option[String] = None, desc: Option[String] = None, price: Option[Int] = None, active: Option[Boolean] = Some(true), img: Option[String] = None)

  /** Table description of table items. Objects of this class serve as prototypes for rows in queries. */
  class Items(_tableTag: Tag) extends profile.api.Table[ItemsRow](_tableTag, Some("gamestore"), "items") {
    def * = (id, name, desc, price, active, img) <> (ItemsRow.tupled, ItemsRow.unapply)

    /** Maps whole row to an option. Useful for outer joins. */
    def ? = ((Rep.Some(id), name, desc, price, active, img)).shaped.<>({ r => import r._; _1.map(_ => ItemsRow.tupled((_1.get, _2, _3, _4, _5, _6))) }, (_: Any) => throw new Exception(NOT_SUPPORTED_TEXT))

    /** Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey */
    val id: Rep[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column name SqlType(VARCHAR), Length(255,true), Default(None) */
    val name: Rep[Option[String]] = column[Option[String]]("name", O.Length(255, varying = true), O.Default(None))
    /** Database column desc SqlType(TEXT), Default(None) */
    val desc: Rep[Option[String]] = column[Option[String]]("desc", O.Default(None))
    /** Database column price SqlType(MEDIUMINT), Default(None) */
    val price: Rep[Option[Int]] = column[Option[Int]]("price", O.Default(None))
    /** Database column active SqlType(BIT), Default(Some(true)) */
    val active: Rep[Option[Boolean]] = column[Option[Boolean]]("active", O.Default(Some(true)))
    /** Database column img SqlType(VARCHAR), Length(255,true), Default(None) */
    val img: Rep[Option[String]] = column[Option[String]]("img", O.Length(255, varying = true), O.Default(None))
  }

  /** Collection-like TableQuery object for table Items */
  lazy val Items = new TableQuery(tag => new Items(tag))

  /** Entity class storing rows of table ItemsOrders
   *
   * @param id      Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey
   * @param itemId  Database column item_id SqlType(MEDIUMINT), Default(None)
   * @param orderId Database column order_id SqlType(MEDIUMINT), Default(None) */
  case class ItemsOrdersRow(id: Int, itemId: Option[Int] = None, orderId: Option[Int] = None)


  /** Table description of table items_orders. Objects of this class serve as prototypes for rows in queries. */
  class ItemsOrders(_tableTag: Tag) extends profile.api.Table[ItemsOrdersRow](_tableTag, Some("gamestore"), "items_orders") {
    def * = (id, itemId, orderId) <> (ItemsOrdersRow.tupled, ItemsOrdersRow.unapply)

    /** Maps whole row to an option. Useful for outer joins. */
    def ? = ((Rep.Some(id), itemId, orderId)).shaped.<>({ r => import r._; _1.map(_ => ItemsOrdersRow.tupled((_1.get, _2, _3))) }, (_: Any) => throw new Exception(NOT_SUPPORTED_TEXT))

    /** Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey */
    val id: Rep[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column item_id SqlType(MEDIUMINT), Default(None) */
    val itemId: Rep[Option[Int]] = column[Option[Int]]("item_id", O.Default(None))
    /** Database column order_id SqlType(MEDIUMINT), Default(None) */
    val orderId: Rep[Option[Int]] = column[Option[Int]]("order_id", O.Default(None))

    /** Foreign key referencing Items (database name items_orders_ibfk_1) */
    lazy val itemsFk = foreignKey("items_orders_ibfk_1", itemId, Items)(r => Rep.Some(r.id), onUpdate = ForeignKeyAction.NoAction, onDelete = ForeignKeyAction.NoAction)
    /** Foreign key referencing Orders (database name items_orders_ibfk_2) */
    lazy val ordersFk = foreignKey("items_orders_ibfk_2", orderId, Orders)(r => Rep.Some(r.id), onUpdate = ForeignKeyAction.NoAction, onDelete = ForeignKeyAction.NoAction)
  }

  /** Collection-like TableQuery object for table ItemsOrders */
  lazy val ItemsOrders = new TableQuery(tag => new ItemsOrders(tag))

  /** Entity class storing rows of table Keys
   *
   * @param id      Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey
   * @param itemId  Database column item_id SqlType(MEDIUMINT), Default(None)
   * @param orderId Database column order_id SqlType(MEDIUMINT), Default(None) */
  case class KeysRow(id: Int, itemId: Option[Int] = None, orderId: Option[Int] = None, key: Option[String] = None)


  /** Table description of table keys. Objects of this class serve as prototypes for rows in queries. */
  class Keys(_tableTag: Tag) extends profile.api.Table[KeysRow](_tableTag, Some("gamestore"), "keys") {
    def * = (id, itemId, orderId, key) <> (KeysRow.tupled, KeysRow.unapply)

    /** Maps whole row to an option. Useful for outer joins. */
    def ? = ((Rep.Some(id), itemId, orderId, key)).shaped.<>({ r => import r._; _1.map(_ => KeysRow.tupled((_1.get, _2, _3, _4))) }, (_: Any) => throw new Exception(NOT_SUPPORTED_TEXT))

    /** Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey */
    val id: Rep[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column item_id SqlType(MEDIUMINT), Default(None) */
    val itemId: Rep[Option[Int]] = column[Option[Int]]("item_id", O.Default(None))
    /** Database column order_id SqlType(MEDIUMINT), Default(None) */
    val orderId: Rep[Option[Int]] = column[Option[Int]]("order_id", O.Default(None))
    /** Database column desc SqlType(TEXT), Default(None) */
    val key: Rep[Option[String]] = column[Option[String]]("key", O.Default(None))

    /** Foreign key referencing Items (database name items_orders_ibfk_1) */
    lazy val itemsFk = foreignKey("items_orders_ibfk_1", itemId, Items)(r => Rep.Some(r.id), onUpdate = ForeignKeyAction.NoAction, onDelete = ForeignKeyAction.NoAction)
    /** Foreign key referencing Orders (database name items_orders_ibfk_2) */
    lazy val ordersFk = foreignKey("items_orders_ibfk_2", orderId, Orders)(r => Rep.Some(r.id), onUpdate = ForeignKeyAction.NoAction, onDelete = ForeignKeyAction.NoAction)
  }

  /** Collection-like TableQuery object for table Keys */
  lazy val Keys = new TableQuery(tag => new Keys(tag))
  /** Entity class storing rows of table Logins
   *
   * @param id       Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey
   * @param email    Database column email SqlType(VARCHAR), Length(255,true), Default(None)
   * @param password Database column password SqlType(VARCHAR), Length(255,true), Default(None) */
  case class LoginsRow(id: Int, email: Option[String] = None, password: Option[String] = None)

  /** Table description of table logins. Objects of this class serve as prototypes for rows in queries. */
  class Logins(_tableTag: Tag) extends profile.api.Table[LoginsRow](_tableTag, Some("gamestore"), "logins") {
    def * = (id, email, password) <> (LoginsRow.tupled, LoginsRow.unapply)

    /** Maps whole row to an option. Useful for outer joins. */
    def ? = ((Rep.Some(id), email, password)).shaped.<>({ r => import r._; _1.map(_ => LoginsRow.tupled((_1.get, _2, _3))) }, (_: Any) => throw new Exception(NOT_SUPPORTED_TEXT))

    /** Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey */
    val id: Rep[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column email SqlType(VARCHAR), Length(255,true), Default(None) */
    val email: Rep[Option[String]] = column[Option[String]]("email", O.Length(255, varying = true), O.Default(None))
    /** Database column password SqlType(VARCHAR), Length(255,true), Default(None) */
    val password: Rep[Option[String]] = column[Option[String]]("password", O.Length(255, varying = true), O.Default(None))

    /** Uniqueness Index over (email) (database name email) */
    val index1 = index("email", email, unique = true)
  }

  /** Collection-like TableQuery object for table Logins */
  lazy val Logins = new TableQuery(tag => new Logins(tag))

  /** Entity class storing rows of table Orders
   *
   * @param id     Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey
   * @param userId Database column user_id SqlType(MEDIUMINT), Default(None)
   * @param when   Database column when SqlType(DATE), Default(None)
   * @param total  Database column total SqlType(MEDIUMINT), Default(None)
   * @param paid   Database column paid SqlType(MEDIUMINT), Default(None)
   * @param link   Database column link SqlType(TEXT), Default(None)
   * @param payid  Database column payId SqlType(TEXT), Default(None) */
  case class OrdersRow(id: Int, userId: Option[Int] = None, when: Option[java.sql.Date] = None, total: Option[Int] = None, paid: Option[Int] = None, link: Option[String] = None, payid: Option[String] = None)

  /** Table description of table orders. Objects of this class serve as prototypes for rows in queries. */
  class Orders(_tableTag: Tag) extends profile.api.Table[OrdersRow](_tableTag, Some("gamestore"), "orders") {
    def * = (id, userId, when, total, paid, link, payid) <> (OrdersRow.tupled, OrdersRow.unapply)

    /** Maps whole row to an option. Useful for outer joins. */
    def ? = ((Rep.Some(id), userId, when, total, paid, link, payid)).shaped.<>({ r => import r._; _1.map(_ => OrdersRow.tupled((_1.get, _2, _3, _4, _5, _6, _7))) }, (_: Any) => throw new Exception(NOT_SUPPORTED_TEXT))

    /** Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey */
    val id: Rep[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column user_id SqlType(MEDIUMINT), Default(None) */
    val userId: Rep[Option[Int]] = column[Option[Int]]("user_id", O.Default(None))
    /** Database column when SqlType(DATE), Default(None) */
    val when: Rep[Option[java.sql.Date]] = column[Option[java.sql.Date]]("when", O.Default(None))
    /** Database column total SqlType(MEDIUMINT), Default(None) */
    val total: Rep[Option[Int]] = column[Option[Int]]("total", O.Default(None))
    /** Database column paid SqlType(MEDIUMINT), Default(None) */
    val paid: Rep[Option[Int]] = column[Option[Int]]("paid", O.Default(None))
    /** Database column link SqlType(TEXT), Default(None) */
    val link: Rep[Option[String]] = column[Option[String]]("link", O.Default(None))
    /** Database column payId SqlType(TEXT), Default(None) */
    val payid: Rep[Option[String]] = column[Option[String]]("payId", O.Default(None))

    /** Foreign key referencing Users (database name orders_ibfk_1) */
    lazy val usersFk = foreignKey("orders_ibfk_1", userId, Users)(r => Rep.Some(r.id), onUpdate = ForeignKeyAction.NoAction, onDelete = ForeignKeyAction.NoAction)
  }

  /** Collection-like TableQuery object for table Orders */
  lazy val Orders = new TableQuery(tag => new Orders(tag))

  /** Entity class storing rows of table Users
   *
   * @param id    Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey
   * @param email Database column email SqlType(VARCHAR), Length(255,true), Default(None)
   * @param name  Database column name SqlType(VARCHAR), Length(255,true), Default(None)
   * @param img   Database column img SqlType(VARCHAR), Length(255,true), Default(None) */
  case class UsersRow(id: Int, email: Option[String] = None, name: Option[String] = None, img: Option[String] = None, isAdmin: Option[Boolean] = Some(false))

  /** Table description of table users. Objects of this class serve as prototypes for rows in queries. */
  class Users(_tableTag: Tag) extends profile.api.Table[UsersRow](_tableTag, Some("gamestore"), "users") {
    def * = (id, email, name, img, isAdmin) <> (UsersRow.tupled, UsersRow.unapply)

    /** Maps whole row to an option. Useful for outer joins. */
    def ? = ((Rep.Some(id), email, name, img, isAdmin)).shaped.<>({ r => import r._; _1.map(_ => UsersRow.tupled((_1.get, _2, _3, _4, _5))) }, (_: Any) => throw new Exception(NOT_SUPPORTED_TEXT))

    /** Database column id SqlType(MEDIUMINT), AutoInc, PrimaryKey */
    val id: Rep[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column email SqlType(VARCHAR), Length(255,true), Default(None) */
    val email: Rep[Option[String]] = column[Option[String]]("email", O.Length(255, varying = true), O.Default(None))
    /** Database column name SqlType(VARCHAR), Length(255,true), Default(None) */
    val name: Rep[Option[String]] = column[Option[String]]("name", O.Length(255, varying = true), O.Default(None))
    /** Database column img SqlType(VARCHAR), Length(255,true), Default(None) */
    val img: Rep[Option[String]] = column[Option[String]]("img", O.Length(255, varying = true), O.Default(None))

    val isAdmin: Rep[Option[Boolean]] = column[Option[Boolean]]("isAdmin", O.Default(Some(false)))

    /** Uniqueness Index over (email) (database name email) */
    val index1 = index("email", email, unique = true)
  }

  /** Collection-like TableQuery object for table Users */
  lazy val Users = new TableQuery(tag => new Users(tag))
}
