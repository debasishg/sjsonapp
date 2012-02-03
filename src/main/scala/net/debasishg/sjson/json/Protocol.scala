package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

import rosetta.json._

trait Writes[T, Json] {
  def writes(o: T): ValidationNEL[String, Json]
}

trait Reads[T, Json] {
  def reads(json: Json): ValidationNEL[String, T]
}

trait Format[T, Json] extends Writes[T, Json] with Reads[T, Json]

trait Protocol[Json] {self: JsonSerialization[Json] =>
  val jsonImplementation: JsonImplementation[Json]
  import jsonImplementation._

  implicit val IntFormat: Format[Int, Json]
  implicit val ShortFormat: Format[Short, Json]
  implicit val LongFormat: Format[Long, Json]
  implicit val BooleanFormat: Format[Boolean, Json]
  implicit val FloatFormat: Format[Float, Json]
  implicit val DoubleFormat: Format[Double, Json]
  implicit val StringFormat: Format[String, Json]

  def field[T](name: String, json: Json)(implicit fjs: Reads[T, Json]): ValidationNEL[String, T] = {
    val JsonObject(m) = json
    m.get(name)
     .map(fromjson[T](_))
     .getOrElse(("field " + name + " not found").fail.liftFailNel)
  }

  def field[T](name: String, json: Json, valid: T => ValidationNEL[String, T])
    (implicit fjs: Reads[T, Json]): ValidationNEL[String, T] = {
    val JsonObject(m) = json
    m.get(name)
     .map(fromjson[T](_).flatMap(valid))
     .getOrElse(("field " + name + " not found").fail.liftFailNel)
  }

  def field_c[T](name: String)(implicit fjs: Reads[T, Json]): Json => ValidationNEL[String, T] = {json: Json =>
    val JsonObject(m) = json
    m.get(name)
     .map(fromjson[T](_))
     .getOrElse(("field " + name + " not found").fail.liftFailNel)
  }
}

trait DefaultProtocol[Json] extends CollectionTypes[Json] 
  with Generic[Json] 
  with Primitives[Json] {self: JsonSerialization[Json] =>
}

/**
object DefaultProtocol extends DefaultProtocol[JsValue] {
  import rosetta.json.dispatch._
  val jsonImplementation = JsonDispatch
}
**/
