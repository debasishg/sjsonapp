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
    def writes(s : S) = (name.success <|*|> tojson(to(s))) match {
      case Success((k: String, v)) => JsonObject((k, v)).success
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
          f1.success <|*|> tojson(product._1),
          f2.success <|*|> tojson(product._2)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
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

  def asProduct3[S, T1,T2,T3](f1: String,f2: String,f3: String)(apply : (T1,T2,T3) => S)(unapply : S => Product3[T1,T2,T3])(implicit bin1: Format[T1, Json],bin2: Format[T2, Json],bin3: Format[T3, Json]) = new Format[S, Json]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          f1.success <|*|> tojson(product._1),
          f2.success <|*|> tojson(product._2),
          f3.success <|*|> tojson(product._3)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
        case Success(kvs) => 
          JsonObject(Map.empty[String, Json] ++ kvs.map{case (k, v) => (k.asInstanceOf[String], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: Json) = js match {
      case m@JsonObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m) |@|
          field[T3](f3, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  def asProduct4[S, T1,T2,T3,T4](f1: String,f2: String,f3: String,f4: String)(apply : (T1,T2,T3,T4) => S)(unapply : S => Product4[T1,T2,T3,T4])(implicit bin1: Format[T1, Json],bin2: Format[T2, Json],bin3: Format[T3, Json],bin4: Format[T4, Json]) = new Format[S, Json]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          f1.success <|*|> tojson(product._1),
          f2.success <|*|> tojson(product._2),
          f3.success <|*|> tojson(product._3),
          f4.success <|*|> tojson(product._4)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
        case Success(kvs) => 
          JsonObject(Map.empty[String, Json] ++ kvs.map{case (k, v) => (k.asInstanceOf[String], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: Json) = js match {
      case m@JsonObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m) |@|
          field[T3](f3, m) |@|
          field[T4](f4, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  def asProduct5[S, T1,T2,T3,T4,T5](f1: String,f2: String,f3: String,f4: String,f5: String)(apply : (T1,T2,T3,T4,T5) => S)(unapply : S => Product5[T1,T2,T3,T4,T5])(implicit bin1: Format[T1, Json],bin2: Format[T2, Json],bin3: Format[T3, Json],bin4: Format[T4, Json],bin5: Format[T5, Json]) = new Format[S, Json]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          f1.success <|*|> tojson(product._1),
          f2.success <|*|> tojson(product._2),
          f3.success <|*|> tojson(product._3),
          f4.success <|*|> tojson(product._4),
          f5.success <|*|> tojson(product._5)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
        case Success(kvs) => 
          JsonObject(Map.empty[String, Json] ++ kvs.map{case (k, v) => (k.asInstanceOf[String], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: Json) = js match {
      case m@JsonObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m) |@|
          field[T3](f3, m) |@|
          field[T4](f4, m) |@|
          field[T5](f5, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  def asProduct6[S, T1,T2,T3,T4,T5,T6](f1: String,f2: String,f3: String,f4: String,f5: String,f6: String)(apply : (T1,T2,T3,T4,T5,T6) => S)(unapply : S => Product6[T1,T2,T3,T4,T5,T6])(implicit bin1: Format[T1, Json],bin2: Format[T2, Json],bin3: Format[T3, Json],bin4: Format[T4, Json],bin5: Format[T5, Json],bin6: Format[T6, Json]) = new Format[S, Json]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          f1.success <|*|> tojson(product._1),
          f2.success <|*|> tojson(product._2),
          f3.success <|*|> tojson(product._3),
          f4.success <|*|> tojson(product._4),
          f5.success <|*|> tojson(product._5),
          f6.success <|*|> tojson(product._6)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
        case Success(kvs) => 
          JsonObject(Map.empty[String, Json] ++ kvs.map{case (k, v) => (k.asInstanceOf[String], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: Json) = js match {
      case m@JsonObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m) |@|
          field[T3](f3, m) |@|
          field[T4](f4, m) |@|
          field[T5](f5, m) |@|
          field[T6](f6, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  def asProduct7[S, T1,T2,T3,T4,T5,T6,T7](f1: String,f2: String,f3: String,f4: String,f5: String,f6: String,f7: String)(apply : (T1,T2,T3,T4,T5,T6,T7) => S)(unapply : S => Product7[T1,T2,T3,T4,T5,T6,T7])(implicit bin1: Format[T1, Json],bin2: Format[T2, Json],bin3: Format[T3, Json],bin4: Format[T4, Json],bin5: Format[T5, Json],bin6: Format[T6, Json],bin7: Format[T7, Json]) = new Format[S, Json]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          f1.success <|*|> tojson(product._1),
          f2.success <|*|> tojson(product._2),
          f3.success <|*|> tojson(product._3),
          f4.success <|*|> tojson(product._4),
          f5.success <|*|> tojson(product._5),
          f6.success <|*|> tojson(product._6),
          f7.success <|*|> tojson(product._7)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
        case Success(kvs) => 
          JsonObject(Map.empty[String, Json] ++ kvs.map{case (k, v) => (k.asInstanceOf[String], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: Json) = js match {
      case m@JsonObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m) |@|
          field[T3](f3, m) |@|
          field[T4](f4, m) |@|
          field[T5](f5, m) |@|
          field[T6](f6, m) |@|
          field[T7](f7, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  def asProduct8[S, T1,T2,T3,T4,T5,T6,T7,T8](f1: String,f2: String,f3: String,f4: String,f5: String,f6: String,f7: String,f8: String)(apply : (T1,T2,T3,T4,T5,T6,T7,T8) => S)(unapply : S => Product8[T1,T2,T3,T4,T5,T6,T7,T8])(implicit bin1: Format[T1, Json],bin2: Format[T2, Json],bin3: Format[T3, Json],bin4: Format[T4, Json],bin5: Format[T5, Json],bin6: Format[T6, Json],bin7: Format[T7, Json],bin8: Format[T8, Json]) = new Format[S, Json]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          f1.success <|*|> tojson(product._1),
          f2.success <|*|> tojson(product._2),
          f3.success <|*|> tojson(product._3),
          f4.success <|*|> tojson(product._4),
          f5.success <|*|> tojson(product._5),
          f6.success <|*|> tojson(product._6),
          f7.success <|*|> tojson(product._7),
          f8.success <|*|> tojson(product._8)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
        case Success(kvs) => 
          JsonObject(Map.empty[String, Json] ++ kvs.map{case (k, v) => (k.asInstanceOf[String], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: Json) = js match {
      case m@JsonObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m) |@|
          field[T3](f3, m) |@|
          field[T4](f4, m) |@|
          field[T5](f5, m) |@|
          field[T6](f6, m) |@|
          field[T7](f7, m) |@|
          field[T8](f8, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
}
