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

  <#list 2..6 as i>
  <#assign typeName>
   Tuple${i}[<#list 1..i as j>T${j} <#if i != j>,</#if></#list>]
  </#assign>
  implicit def tuple${i}Format[<#list 1..i as j>T${j}<#if i !=j>,</#if></#list>](implicit
    <#list 1..i as j>
      fmt${j}: Format[T${j}, Json]<#if i != j>,</#if>
    </#list>
    ): Format[${typeName}, Json] = new Format[${typeName}, Json]{
    def reads (json: Json): Validation[NonEmptyList[String], ${typeName}] = {
      val JsonArray(<#list 1..i as j>e${j}::</#list> Nil) = json
      (
    <#list 1..i as j>
    fromjson[T${j}](e${j})<#if i != j>|@|</#if>
    </#list>
      ).tupled
    }

    def writes(tuple: ${typeName}) = tuple match {
      case (<#list 1..i as j>t${j}<#if i != j>,</#if></#list>) =>
        val l = List(
      <#list 1..i as j>tojson(t${j})(fmt${j})<#if i != j>,</#if></#list>).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, Json]
        l match {
          case Success(js) => JsonArray(js).success
          case Failure(errs) => errs.fail
        }
      case _ => ("Tuple" + ${i} + " expected").fail.liftFailNel
    }
  }
  </#list>
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

  import jsonImplementation._

  implicit object BigIntFormat extends Format[BigInt, Json] {
    def writes(o: BigInt) = JsonLong.apply(o.longValue).success
    def reads(json: Json) = json match {
      case JsonLong(n) => BigInt(n).success
      case _ => "BigInt expected".fail.liftFailNel
    }
  }

  implicit object BigDecimalFormat extends Format[BigDecimal, Json] {
    def writes(o: BigDecimal) = JsonDouble.apply(o.doubleValue).success
    def reads(json: Json) = json match {
      case JsonDouble(n) => BigDecimal(n).success
      case _ => "BigDecimal expected".fail.liftFailNel
    }
  }
}

object BasicTypes {
  /** 2.7/8 compatibility */
  implicit def orderable[A](implicit s: A => Ordered[A]): scala.math.Ordering[A] = new scala.math.Ordering[A] {
    def compare(x: A, y: A) = s(x).compare(y)
  }
}
