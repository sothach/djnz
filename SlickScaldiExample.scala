import scaldi.{Injector, Module}
import scaldi.Injectable._
import scala.slick.driver.MySQLDriver.simple._

/* these are the domain model classes */
case class Coffee(name: String, price: Double)(supplier: String)
case class Supplier(id: Int, name: String)

/* classes that map the physical schema to either domain model classes,
 * or, in the case of 'Coffees', to a tuple view of the table row: this
 * is necessary because we need to join coffees+suppliers to construct
 * a complete domain Coffee object */
class Coffees(tag: Tag)
  extends Table[(String, Int, Double)](tag, "COFFEES") {
  def name = column[String]("COF_NAME")
  def supplierId = column[Int]("SUP_ID")
  def price = column[Double]("PRICE")
  override def * = (name, supplierId, price)
}

class Suppliers(val tag: Tag) extends Table[Supplier](tag, "SUPPLIERS") {
  def id = column[Int]("SUP_ID")
  def name = column[String]("SUP_NAME")
  def * = (id, name) <>(Supplier.tupled, Supplier.unapply)
}

/* the (generic) service interface to query the database */
trait CoffeeQuery {
  def listAll: List[Coffee]
}

/* a specific query implementation using a MySQL database */
class MySqlCoffeeQuery(implicit inj: Injector) extends CoffeeQuery {
  private val db = inject [Database]
  private val limit = inject [Int]
  val coffees = TableQuery[Coffees]
  val suppliers = TableQuery[Suppliers]
  lazy val joined = coffees join suppliers on (_.supplierId === _.id)

  override def listAll: List[Coffee] =
    db.withTransaction { implicit session =>
      (joined list) map {
        case (c, s) => Coffee(c._1, c._3)(s.name)
      } take limit
    }

}

/* bind the abstract service interface to the concrete implementation wwe want to use */
class QueryModule extends Module {
  bind [CoffeeQuery] to new MySqlCoffeeQuery
}

/* configure the database and query limit parameters for the query module */
class Configuration extends Module {
  bind [Database] to Database.forConfig("pvddb")
  binding identifiedBy "limit" to 12
}

/* instantiate the query module and call its `listAll` query */
object SlickScaldiExample extends App {
  implicit val modules = new QueryModule :: new Configuration
  val query = inject [CoffeeQuery]
  query.listAll foreach println
}
