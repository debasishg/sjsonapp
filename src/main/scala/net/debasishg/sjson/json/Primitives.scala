package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

trait Primitives[Json] extends Protocol[Json] {self: JsonSerialization[Json] =>
  import jsonImplementation._

  implicit object IntFormat extends Format[Int, Json] {
    def writes(o: Int) = JsonLong(o).success
    def reads(json: Json) = json match {
      case JsonLong(n) => n.intValue.success
      case _ => "Int expected".fail.liftFailNel
    }
  }

  implicit object ShortFormat extends Format[Short, Json] {
    def writes(o: Short) = JsonLong(o).success
    def reads(json: Json) = json match {
      case JsonLong(n) => n.shortValue.success
      case _ => "Short expected".fail.liftFailNel
    }
  }

  implicit object FloatFormat extends Format[Float, Json] {
    def writes(o: Float) = JsonDouble(o.toDouble).success
    def reads(json: Json) = json match {
      case JsonDouble(n) => n.floatValue.success
      case _ => "Float expected".fail.liftFailNel
    }
  }

  implicit object LongFormat extends Format[Long, Json] {
    def writes(o: Long) = JsonLong(o).success
    def reads(json: Json) = json match {
      case JsonLong(n) => n.success
      case _ => "Long expected".fail.liftFailNel
    }
  }

  implicit object DoubleFormat extends Format[Double, Json] {
    def writes(o: Double) = JsonDouble(o).success
    def reads(json: Json) = json match {
      case JsonDouble(n) => n.success
      case _ => "Double expected".fail.liftFailNel
    }
  }

  implicit object BooleanFormat extends Format[Boolean, Json] {
    def writes(o: Boolean) = JsonBool(o).success
    def reads(json: Json) = json match { 
      case JsonBool(true) => true.success
      case JsonBool(false) => false.success
      case _ => "Boolean expected".fail.liftFailNel
    }
  }

  implicit object StringFormat extends Format[String, Json] {
    def writes(o: String) = JsonString(o).success
    def reads(json: Json) = json match {
      case JsonString(s) => s.success
      case _ => "String expected".fail.liftFailNel
    }
  }

  implicit object JsValueFormat extends Format[Json, Json] {
    def writes(o: Json) = o.success
    def reads(json: Json) = json.success
  }
}
