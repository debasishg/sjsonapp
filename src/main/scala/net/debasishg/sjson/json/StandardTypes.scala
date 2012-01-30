package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

import dispatch.json._
import JsonSerialization._

trait BasicTypes extends Protocol {
  implicit def optionFormat[T](implicit fmt : Format[T]) : Format[Option[T]] = new Format[Option[T]] {
    def writes(ot: Option[T]) = ot match {
      case Some(t) => tojson(t)
      case None => JsNull.success
    }
    def reads(json: JsValue) = json match {
      case JsNull => None.success
      case x => some(fromjson[T](x)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]
    }
  }
    implicit def tuple2Format[T1,T2](implicit 
      fmt1: Format[T1],
      fmt2: Format[T2]
    ): Format[   Tuple2[T1 ,T2 ]
] = new Format[   Tuple2[T1 ,T2 ]
]{
      def reads (json: JsValue): Validation[NonEmptyList[String],    Tuple2[T1 ,T2 ]
] = {
        val JsArray(e1::e2:: Nil) = json
        (
    fromjson[T1](e1)|@|
    fromjson[T2](e2)
        ).tupled
      }
      def writes(tuple:    Tuple2[T1 ,T2 ]
) = tuple match {
        case (t1,t2) => 
          val l = List(
      tojson(t1)(fmt1),tojson(t2)(fmt2)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, JsValue]
          l match {
            case Success(js) => JsArray(js).success
            case Failure(errs) => errs.fail 
          }
        case _ => ("Tuple" + 2 + " expected").fail.liftFailNel
      }
  }
  implicit def tuple3Format[T1,T2,T3](implicit 
      fmt1: Format[T1],
      fmt2: Format[T2],
      fmt3: Format[T3]
    ): Format[   Tuple3[T1 ,T2 ,T3 ]
] = new Format[   Tuple3[T1 ,T2 ,T3 ]
]{
      def reads (json: JsValue): Validation[NonEmptyList[String],    Tuple3[T1 ,T2 ,T3 ]
] = {
        val JsArray(e1::e2::e3:: Nil) = json
        (
    fromjson[T1](e1)|@|
    fromjson[T2](e2)|@|
    fromjson[T3](e3)
        ).tupled
      }
      def writes(tuple:    Tuple3[T1 ,T2 ,T3 ]
) = tuple match {
        case (t1,t2,t3) => 
          val l = List(
      tojson(t1)(fmt1),tojson(t2)(fmt2),tojson(t3)(fmt3)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, JsValue]
          l match {
            case Success(js) => JsArray(js).success
            case Failure(errs) => errs.fail 
          }
        case _ => ("Tuple" + 3 + " expected").fail.liftFailNel
      }
  }
  implicit def tuple4Format[T1,T2,T3,T4](implicit 
      fmt1: Format[T1],
      fmt2: Format[T2],
      fmt3: Format[T3],
      fmt4: Format[T4]
    ): Format[   Tuple4[T1 ,T2 ,T3 ,T4 ]
] = new Format[   Tuple4[T1 ,T2 ,T3 ,T4 ]
]{
      def reads (json: JsValue): Validation[NonEmptyList[String],    Tuple4[T1 ,T2 ,T3 ,T4 ]
] = {
        val JsArray(e1::e2::e3::e4:: Nil) = json
        (
    fromjson[T1](e1)|@|
    fromjson[T2](e2)|@|
    fromjson[T3](e3)|@|
    fromjson[T4](e4)
        ).tupled
      }
      def writes(tuple:    Tuple4[T1 ,T2 ,T3 ,T4 ]
) = tuple match {
        case (t1,t2,t3,t4) => 
          val l = List(
      tojson(t1)(fmt1),tojson(t2)(fmt2),tojson(t3)(fmt3),tojson(t4)(fmt4)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, JsValue]
          l match {
            case Success(js) => JsArray(js).success
            case Failure(errs) => errs.fail 
          }
        case _ => ("Tuple" + 4 + " expected").fail.liftFailNel
      }
  }
  implicit def tuple5Format[T1,T2,T3,T4,T5](implicit 
      fmt1: Format[T1],
      fmt2: Format[T2],
      fmt3: Format[T3],
      fmt4: Format[T4],
      fmt5: Format[T5]
    ): Format[   Tuple5[T1 ,T2 ,T3 ,T4 ,T5 ]
] = new Format[   Tuple5[T1 ,T2 ,T3 ,T4 ,T5 ]
]{
      def reads (json: JsValue): Validation[NonEmptyList[String],    Tuple5[T1 ,T2 ,T3 ,T4 ,T5 ]
] = {
        val JsArray(e1::e2::e3::e4::e5:: Nil) = json
        (
    fromjson[T1](e1)|@|
    fromjson[T2](e2)|@|
    fromjson[T3](e3)|@|
    fromjson[T4](e4)|@|
    fromjson[T5](e5)
        ).tupled
      }
      def writes(tuple:    Tuple5[T1 ,T2 ,T3 ,T4 ,T5 ]
) = tuple match {
        case (t1,t2,t3,t4,t5) => 
          val l = List(
      tojson(t1)(fmt1),tojson(t2)(fmt2),tojson(t3)(fmt3),tojson(t4)(fmt4),tojson(t5)(fmt5)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, JsValue]
          l match {
            case Success(js) => JsArray(js).success
            case Failure(errs) => errs.fail 
          }
        case _ => ("Tuple" + 5 + " expected").fail.liftFailNel
      }
  }
  implicit def tuple6Format[T1,T2,T3,T4,T5,T6](implicit 
      fmt1: Format[T1],
      fmt2: Format[T2],
      fmt3: Format[T3],
      fmt4: Format[T4],
      fmt5: Format[T5],
      fmt6: Format[T6]
    ): Format[   Tuple6[T1 ,T2 ,T3 ,T4 ,T5 ,T6 ]
] = new Format[   Tuple6[T1 ,T2 ,T3 ,T4 ,T5 ,T6 ]
]{
      def reads (json: JsValue): Validation[NonEmptyList[String],    Tuple6[T1 ,T2 ,T3 ,T4 ,T5 ,T6 ]
] = {
        val JsArray(e1::e2::e3::e4::e5::e6:: Nil) = json
        (
    fromjson[T1](e1)|@|
    fromjson[T2](e2)|@|
    fromjson[T3](e3)|@|
    fromjson[T4](e4)|@|
    fromjson[T5](e5)|@|
    fromjson[T6](e6)
        ).tupled
      }
      def writes(tuple:    Tuple6[T1 ,T2 ,T3 ,T4 ,T5 ,T6 ]
) = tuple match {
        case (t1,t2,t3,t4,t5,t6) => 
          val l = List(
      tojson(t1)(fmt1),tojson(t2)(fmt2),tojson(t3)(fmt3),tojson(t4)(fmt4),tojson(t5)(fmt5),tojson(t6)(fmt6)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, JsValue]
          l match {
            case Success(js) => JsArray(js).success
            case Failure(errs) => errs.fail 
          }
        case _ => ("Tuple" + 6 + " expected").fail.liftFailNel
      }
  }
}

trait CollectionTypes extends BasicTypes with Generic {

  implicit def listFormat[T](implicit fmt : Format[T]) : Format[List[T]] = new Format[List[T]] {
    def writes(ts: List[T]) =
      ts.map(t => tojson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, JsValue] match {
        case Success(js) => JsArray(js).success
        case Failure(errs) => errs.fail
      }
    def reads(json: JsValue) = json match {
      case JsArray(ts) => ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]
      case _ => "List expected".fail.liftFailNel
    }
  }

  implicit def seqFormat[T](implicit fmt : Format[T]) : Format[Seq[T]] = new Format[Seq[T]] {
    def writes(ts: Seq[T]) =
      ts.toList.map(t => tojson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, JsValue] match {
        case Success(js) => JsArray(js).success
        case Failure(errs) => errs.fail
      }
    def reads(json: JsValue) = json match {
      case JsArray(ts) => ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]
      case _ => "Seq expected".fail.liftFailNel
    }
  }

  import scala.reflect.Manifest
  implicit def arrayFormat[T](implicit fmt : Format[T], mf: Manifest[T]) : Format[Array[T]] = new Format[Array[T]] {
    def writes(ts: Array[T]) =
      ts.map(t => tojson(t)(fmt)).toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, JsValue] match {
        case Success(js) => JsArray(js).success
        case Failure(errs) => errs.fail
      }
    def reads(json: JsValue) = json match {
      case JsArray(ts) => (ts.map(t => fromjson(t)(fmt)).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, T]).map(listToArray(_))
      case _ => "Array expected".fail.liftFailNel
    }
  }

  def listToArray[T: Manifest](ls: List[T]): Array[T] = ls.toArray

  implicit def mapFormat[K, V](implicit fmtk: Format[K], fmtv: Format[V]) : Format[Map[K, V]] = new Format[Map[K, V]] {
    def writes(ts: Map[K, V]) =
      ts.map{ case(k, v) =>
        (tojson(k.toString) <|*|> tojson(v)(fmtv))
      }.toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
        case Success(kvs) => JsObject(kvs.map{case (key, value) => (key.asInstanceOf[JsString], value)}).success
        case Failure(errs) => errs.fail
      }
    def reads(json: JsValue) = json match {
      case JsObject(m) =>
        val Success(keys) =
          m.map{ case (k, v) => fromjson[K](k)(fmtk)}.toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, K]
        val Success(values) =
          m.map{ case (k, v) => fromjson[V](v)(fmtv)}.toList.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, V]
        (Map() ++ (keys zip values)).success[NonEmptyList[String]]
      case _ => "Map expected".fail.liftFailNel
    }
  }

/**
  import scala.collection._
  implicit def mutableSetFormat[T](implicit fmt: Format[T]): Format[mutable.Set[T]] = 
    viaSeq((x: Seq[T]) => mutable.Set(x: _*))

  implicit def immutableSetFormat[T](implicit fmt: Format[T]): Format[immutable.Set[T]] = 
    viaSeq((x: Seq[T]) => immutable.Set(x: _*))

  implicit def immutableSortedSetFormat[S](implicit ord : S => Ordered[S], binS : Format[S]) : Format[immutable.SortedSet[S]] = {
    import BasicTypes.orderable // 2.7/8 compatibility
    viaSeq((x: Seq[S]) => immutable.TreeSet[S](x: _*))
  }
**/
}

trait StandardTypes extends CollectionTypes {

  implicit object BigIntFormat extends Format[BigInt] {
    def writes(o: BigInt) = JsValue.apply(o).success
    def reads(json: JsValue) = json match {
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
}

object BasicTypes {
  /** 2.7/8 compatibility */
  implicit def orderable[A](implicit s: A => Ordered[A]): scala.math.Ordering[A] = new scala.math.Ordering[A] {
    def compare(x: A, y: A) = s(x).compare(y)
  }
}
