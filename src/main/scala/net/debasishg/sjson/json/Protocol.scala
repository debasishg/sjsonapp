package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

import dispatch.json._
import JsonSerialization._

trait Writes[T] {
  def writes(o: T): ValidationNEL[String, JsValue]
}

trait Reads[T] {
  def reads(json: JsValue): ValidationNEL[String, T]
}

trait Format[T] extends Writes[T] with Reads[T]

trait Protocol {
  implicit val IntFormat: Format[Int]
  implicit val ShortFormat: Format[Short]
  implicit val LongFormat: Format[Long]
  implicit val BooleanFormat: Format[Boolean]
  implicit val FloatFormat: Format[Float]
  implicit val DoubleFormat: Format[Double]
  implicit val StringFormat: Format[String]
}

trait DefaultProtocol extends CollectionTypes with Generic with Primitives
object DefaultProtocol extends DefaultProtocol {
  def field[T: Reads](name: String, json: JsValue): ValidationNEL[String, T] = {
    val JsObject(m) = json
    m.get(JsString(name))
     .map(fromjson[T](_))
     .getOrElse(("field " + name + " not found").fail.liftFailNel)
  }

  def field[T: Reads](name: String, json: JsValue, valid: T => ValidationNEL[String, T]): ValidationNEL[String, T] = {
    val JsObject(m) = json
    m.get(JsString(name))
     .map(fromjson[T](_).flatMap(valid))
     .getOrElse(("field " + name + " not found").fail.liftFailNel)
  }

  def field_c[T: Reads](name: String): JsValue => ValidationNEL[String, T] = {json: JsValue =>
    val JsObject(m) = json
    m.get(JsString(name))
     .map(fromjson[T](_))
     .getOrElse(("field " + name + " not found").fail.liftFailNel)
  }
}
