package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

trait BasicTypes[Json] extends Protocol[Json] {self: JsonSerialization[Json] =>
  import jsonImplementation._

  implicit def optionFormat[T](implicit fmt : Format[T, Json]) : Format[Option[T], Json] = new Format[Option[T], Json] {
    def writes(ot: Option[T]) = ot match {
      case Some(t) => tojson(t)
      case None => JsonNull.success
    }
    def reads(json: Json) = json match {
      case JsonNull => None.success
      case x => some(fromjson[T](x)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]
    }
  }
    implicit def tuple2Format[T1,T2](implicit 
      fmt1: Format[T1, Json],
      fmt2: Format[T2, Json]
    ): Format[   Tuple2[T1 ,T2 ], Json] = new Format[Tuple2[T1 ,T2], Json]{
      def reads (json: Json): Validation[NonEmptyList[String],    Tuple2[T1 ,T2 ]
] = {
        val JsonArray(e1::e2:: Nil) = json
        (
    fromjson[T1](e1)|@|
    fromjson[T2](e2)
        ).tupled
      }
      def writes(tuple:    Tuple2[T1 ,T2 ]
) = tuple match {
        case (t1,t2) => 
          val l = List(
      tojson(t1)(fmt1),tojson(t2)(fmt2)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, Json]
          l match {
            case Success(js) => JsonArray(js).success
            case Failure(errs) => errs.fail 
          }
        case _ => ("Tuple" + 2 + " expected").fail.liftFailNel
      }
  }
}

trait CollectionTypes[Json] extends BasicTypes[Json] with Generic[Json] {self: JsonSerialization[Json] =>
  import jsonImplementation._

  implicit def listFormat[T](implicit fmt : Format[T, Json]) : Format[List[T], Json] = new Format[List[T], Json] {
    def writes(ts: List[T]) =
      ts.map(t => tojson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, Json] match {
        case Success(js) => JsonArray(js).success
        case Failure(errs) => errs.fail
      }
    def reads(json: Json) = json match {
      case JsonArray(ts) => ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]
      case _ => "List expected".fail.liftFailNel
    }
  }

  implicit def seqFormat[T](implicit fmt : Format[T, Json]) : Format[Seq[T], Json] = new Format[Seq[T], Json] {
    def writes(ts: Seq[T]) =
      ts.toList.map(t => tojson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, Json] match {
        case Success(js) => JsonArray(js).success
        case Failure(errs) => errs.fail
      }
    def reads(json: Json) = json match {
      case JsonArray(ts) => ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]
      case _ => "Seq expected".fail.liftFailNel
    }
  }

  import scala.reflect.Manifest
  implicit def arrayFormat[T](implicit fmt : Format[T, Json], mf: Manifest[T]) : Format[Array[T], Json] = new Format[Array[T], Json] {
    def writes(ts: Array[T]) =
      ts.map(t => tojson(t)(fmt)).toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, Json] match {
        case Success(js) => JsonArray(js).success
        case Failure(errs) => errs.fail
      }
    def reads(json: Json) = json match {
      case JsonArray(ts) => (ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]).map(listToArray(_))
      case _ => "Array expected".fail.liftFailNel
    }
  }

  def listToArray[T: Manifest](ls: List[T]): Array[T] = ls.toArray

  implicit def mapFormat[V](implicit fmtv: Format[V, Json]): Format[Map[String, V], Json] = new Format[Map[String, V], Json] {
    def writes(ts: Map[String, V]) =
      ts.map{ case(k, v) =>
        (k.success <|*|> tojson(v)(fmtv))
      }.toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
        case Success(kvs) => JsonObject(kvs).success
        case Failure(errs) => errs.fail
      }
    def reads(json: Json) = json match {
      case JsonObject(m) =>
        val Success(values) =
          m.map{ case (k: String, v) => fromjson[V](v)(fmtv)}.toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, V]
          (Map.empty[String, V] ++ (m.keySet.toList zip values)).success[NonEmptyList[String]]
      case _ => "Map expected".fail.liftFailNel
    }
  }
}

trait StandardTypes[Json] extends CollectionTypes[Json] {self: JsonSerialization[Json] =>

/**
  implicit object BigIntFormat extends Format[BigInt] {
    def writes(o: BigInt) = JsValue.apply(o).success
    def reads(json: Json) = json match {
      case JsNumber(n) => n.toBigInt.success
      case _ => "BigInt expected".fail.liftFailNel
    }
  }

  implicit object BigDecimalFormat extends Format[BigDecimal] {
    def writes(o: BigDecimal) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
      case JsNumber(n) => n.success
      case _ => "BigDecimal expected".fail.liftFailNel
    }
  }
**/
}

object BasicTypes {
  /** 2.7/8 compatibility */
  implicit def orderable[A](implicit s: A => Ordered[A]): scala.math.Ordering[A] = new scala.math.Ordering[A] {
    def compare(x: A, y: A) = s(x).compare(y)
  }
}
