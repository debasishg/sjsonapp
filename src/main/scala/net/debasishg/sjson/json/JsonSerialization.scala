package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

import rosetta.json._

trait JsonSerialization[Json] {
  def tojson[T](o: T)(implicit tjs: Writes[T, Json]): ValidationNEL[String, Json] = tjs.writes(o)

  def fromjson[T](json: Json)(implicit fjs: Reads[T, Json]): ValidationNEL[String, T] = fjs.reads(json)

  // def tobinary[T](o: T)(implicit tjs: Writes[T, Json]): ValidationNEL[String, Array[Byte]] = 
    // tojson(o) map JsValue.toJson map (s => s.getBytes("UTF-8"))

  // def frombinary[T](bytes: Array[Byte])(implicit fjs: Reads[T]): ValidationNEL[String, T] =
    // fromjson(Js(new String(bytes, "UTF-8")))
}
