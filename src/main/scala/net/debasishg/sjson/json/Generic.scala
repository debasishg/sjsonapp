package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

trait Generic[Json] extends Protocol[Json] {self: JsonSerialization[Json] =>

  import jsonImplementation._

  implicit def listFormat[T](implicit fmt: Format[T, Json]): Format[List[T], Json];

  /**
   * Use this when you would wrap a value in a case class
   *
   * <pre>
   * case class Name(name: String)
   * implicit val NameFormat: Format[Name] = wrap[Name, String]("name")(_.name, Name)
   *
   * val n = Name("debasish ghosh")
   * fromjson[Name](tojson(n)) should equal(n)
   * </pre>
   */
  def wrap[S, T](name: String)(to : S => T, from : T => S)(implicit fmt : Format[T, Json]) = new Format[S, Json]{
    def writes(s : S) = (tojson(name) <|*|> tojson(to(s))) match {
      case Success((JsonString(k), v)) => JsonObject((k, v)).success
      case Failure(errs) => errs.fail
    }
    def reads(js : Json) = js match {
      case JsonObject(_) =>
        val f = field[T](name, js)
        f match {
          case Success(v) => from(v).success
          case Failure(e) => e.fail
        }
      case _ => "Object expected".fail.liftFailNel
    }
  }

  /**
   * Lazy wrapper around serialization. Useful when you want to serialize mutually recursive structures.
   */
  def lazyFormat[S](fmt : => Format[S, Json]) = new Format[S, Json]{
    lazy val delegate = fmt;

    def reads(js : Json) = delegate.reads(js);
    def writes(s : S) = delegate.writes(s);
  }

  def asProduct2[S, T1,T2](f1: String,f2: String)(apply : (T1,T2) => S)(unapply : S => Product2[T1,T2])(implicit bin1: Format[T1, Json],bin2: Format[T2, Json]) = new Format[S, Json]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          tojson(f1) <|*|> tojson(product._1),
          tojson(f2) <|*|> tojson(product._2)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (Json, Json)] match {
        case Success(kvs) => 
          JsonObject(Map.empty[String, Json] ++ kvs.map{case (k, v) => (k.asInstanceOf[String], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: Json) = js match {
      case m@JsonObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
}
