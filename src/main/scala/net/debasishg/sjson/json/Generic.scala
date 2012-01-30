package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

import dispatch.json._
trait Generic extends Protocol {

  import JsonSerialization._
  import DefaultProtocol._

  implicit def listFormat[T](implicit fmt : Format[T]) : Format[List[T]];

/**
  def viaSeq[S <: Iterable[T], T] (f: Seq[T] => S) (implicit fmt : Format[T]) : Format[S] = new Format[S] {
    def writes(ts: S) = JsArray(ts.map(t => tojson(t)(fmt)).toList)
    def reads(json: JsValue) = json match {
      case JsArray(ts) => f(ts.map(t => fromjson[T](t)))
      case _ => throw new RuntimeException("Collection expected")
    }
  }
**/

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
  def wrap[S, T](name: String)(to : S => T, from : T => S)(implicit fmt : Format[T]) = new Format[S]{
    def writes(s : S) = (tojson(name) <|*|> tojson(to(s))) match {
      case Success((k, v)) => JsObject(Map() ++ List((k.asInstanceOf[JsString], v))).success
      case Failure(errs) => errs.fail
    }
    def reads(js : JsValue) = js match {
      case m@JsObject(_) =>
        val f = field[T](name, m)
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
  def lazyFormat[S](fmt : => Format[S]) = new Format[S]{
    lazy val delegate = fmt;

    def reads(js : JsValue) = delegate.reads(js);
    def writes(s : S) = delegate.writes(s);
  }

  def asProduct2[S, T1,T2](f1: String,f2: String)(apply : (T1,T2) => S)(unapply : S => Product2[T1,T2])(implicit bin1: Format[T1],bin2: Format[T2]) = new Format[S]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          tojson(f1) <|*|> tojson(product._1),
          tojson(f2) <|*|> tojson(product._2)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
        case Success(kvs) => 
          JsObject(Map.empty[JsString, JsValue] ++ kvs.map{case (k, v) => (k.asInstanceOf[JsString], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: JsValue) = js match {
      case m@JsObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  def asProduct3[S, T1,T2,T3](f1: String,f2: String,f3: String)(apply : (T1,T2,T3) => S)(unapply : S => Product3[T1,T2,T3])(implicit bin1: Format[T1],bin2: Format[T2],bin3: Format[T3]) = new Format[S]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          tojson(f1) <|*|> tojson(product._1),
          tojson(f2) <|*|> tojson(product._2),
          tojson(f3) <|*|> tojson(product._3)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
        case Success(kvs) => 
          JsObject(Map.empty[JsString, JsValue] ++ kvs.map{case (k, v) => (k.asInstanceOf[JsString], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: JsValue) = js match {
      case m@JsObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m) |@|
          field[T3](f3, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  def asProduct4[S, T1,T2,T3,T4](f1: String,f2: String,f3: String,f4: String)(apply : (T1,T2,T3,T4) => S)(unapply : S => Product4[T1,T2,T3,T4])(implicit bin1: Format[T1],bin2: Format[T2],bin3: Format[T3],bin4: Format[T4]) = new Format[S]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          tojson(f1) <|*|> tojson(product._1),
          tojson(f2) <|*|> tojson(product._2),
          tojson(f3) <|*|> tojson(product._3),
          tojson(f4) <|*|> tojson(product._4)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
        case Success(kvs) => 
          JsObject(Map.empty[JsString, JsValue] ++ kvs.map{case (k, v) => (k.asInstanceOf[JsString], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: JsValue) = js match {
      case m@JsObject(_) => // m is the Map
        (
          field[T1](f1, m) |@|
          field[T2](f2, m) |@|
          field[T3](f3, m) |@|
          field[T4](f4, m)
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  def asProduct5[S, T1,T2,T3,T4,T5](f1: String,f2: String,f3: String,f4: String,f5: String)(apply : (T1,T2,T3,T4,T5) => S)(unapply : S => Product5[T1,T2,T3,T4,T5])(implicit bin1: Format[T1],bin2: Format[T2],bin3: Format[T3],bin4: Format[T4],bin5: Format[T5]) = new Format[S]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          tojson(f1) <|*|> tojson(product._1),
          tojson(f2) <|*|> tojson(product._2),
          tojson(f3) <|*|> tojson(product._3),
          tojson(f4) <|*|> tojson(product._4),
          tojson(f5) <|*|> tojson(product._5)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
        case Success(kvs) => 
          JsObject(Map.empty[JsString, JsValue] ++ kvs.map{case (k, v) => (k.asInstanceOf[JsString], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: JsValue) = js match {
      case m@JsObject(_) => // m is the Map
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
  def asProduct6[S, T1,T2,T3,T4,T5,T6](f1: String,f2: String,f3: String,f4: String,f5: String,f6: String)(apply : (T1,T2,T3,T4,T5,T6) => S)(unapply : S => Product6[T1,T2,T3,T4,T5,T6])(implicit bin1: Format[T1],bin2: Format[T2],bin3: Format[T3],bin4: Format[T4],bin5: Format[T5],bin6: Format[T6]) = new Format[S]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          tojson(f1) <|*|> tojson(product._1),
          tojson(f2) <|*|> tojson(product._2),
          tojson(f3) <|*|> tojson(product._3),
          tojson(f4) <|*|> tojson(product._4),
          tojson(f5) <|*|> tojson(product._5),
          tojson(f6) <|*|> tojson(product._6)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
        case Success(kvs) => 
          JsObject(Map.empty[JsString, JsValue] ++ kvs.map{case (k, v) => (k.asInstanceOf[JsString], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: JsValue) = js match {
      case m@JsObject(_) => // m is the Map
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
  def asProduct7[S, T1,T2,T3,T4,T5,T6,T7](f1: String,f2: String,f3: String,f4: String,f5: String,f6: String,f7: String)(apply : (T1,T2,T3,T4,T5,T6,T7) => S)(unapply : S => Product7[T1,T2,T3,T4,T5,T6,T7])(implicit bin1: Format[T1],bin2: Format[T2],bin3: Format[T3],bin4: Format[T4],bin5: Format[T5],bin6: Format[T6],bin7: Format[T7]) = new Format[S]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          tojson(f1) <|*|> tojson(product._1),
          tojson(f2) <|*|> tojson(product._2),
          tojson(f3) <|*|> tojson(product._3),
          tojson(f4) <|*|> tojson(product._4),
          tojson(f5) <|*|> tojson(product._5),
          tojson(f6) <|*|> tojson(product._6),
          tojson(f7) <|*|> tojson(product._7)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
        case Success(kvs) => 
          JsObject(Map.empty[JsString, JsValue] ++ kvs.map{case (k, v) => (k.asInstanceOf[JsString], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: JsValue) = js match {
      case m@JsObject(_) => // m is the Map
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
  def asProduct8[S, T1,T2,T3,T4,T5,T6,T7,T8](f1: String,f2: String,f3: String,f4: String,f5: String,f6: String,f7: String,f8: String)(apply : (T1,T2,T3,T4,T5,T6,T7,T8) => S)(unapply : S => Product8[T1,T2,T3,T4,T5,T6,T7,T8])(implicit bin1: Format[T1],bin2: Format[T2],bin3: Format[T3],bin4: Format[T4],bin5: Format[T5],bin6: Format[T6],bin7: Format[T7],bin8: Format[T8]) = new Format[S]{
    def writes(s: S) = {
      val product = unapply(s)
      List(
          tojson(f1) <|*|> tojson(product._1),
          tojson(f2) <|*|> tojson(product._2),
          tojson(f3) <|*|> tojson(product._3),
          tojson(f4) <|*|> tojson(product._4),
          tojson(f5) <|*|> tojson(product._5),
          tojson(f6) <|*|> tojson(product._6),
          tojson(f7) <|*|> tojson(product._7),
          tojson(f8) <|*|> tojson(product._8)
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
        case Success(kvs) => 
          JsObject(Map.empty[JsString, JsValue] ++ kvs.map{case (k, v) => (k.asInstanceOf[JsString], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: JsValue) = js match {
      case m@JsObject(_) => // m is the Map
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
