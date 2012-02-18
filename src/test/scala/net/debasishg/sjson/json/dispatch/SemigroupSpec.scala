package net.debasishg
package sjson
package json
package dispatchprotocol

import org.scalatest.{Spec, BeforeAndAfterEach, BeforeAndAfterAll}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalaz._
import Scalaz._

import dispatch.json._
import rosetta.json.dispatch._
import Semigroups._

@RunWith(classOf[JUnitRunner])
class TypeclassSpec extends Spec 
                with ShouldMatchers
                with BeforeAndAfterEach
                with BeforeAndAfterAll
                with JsonSerialization[JsValue] with DefaultProtocol[JsValue] {

  val jsonImplementation = JsonDispatch

  describe("JsString as Semigroup") {
    it("should append") {
      val s1 = JsString("debasish")
      val s2 = JsString("ghosh")
      (s1 |+| s2) should equal(JsString("debasishghosh"))
    }
  }

  describe("JsNumber as Semigroup") {
    it("should append") {
      val s1 = JsNumber(10)
      val s2 = JsNumber(20)
      (s1 |+| s2) should equal(JsNumber(30))
    }
  }

  describe("JsArray as Semigroup") {
    it("should append") {
      val s1 = JsArray(List(JsNumber(1), JsNumber(2), JsNumber(3)))
      val s2 = JsArray(List(JsNumber(10), JsNumber(20), JsNumber(30)))
      (s1 |+| s2) should equal(JsArray(List(JsNumber(1), JsNumber(2), JsNumber(3), JsNumber(10), JsNumber(20), JsNumber(30))))
    }
  }

  describe("JsObject as Semigroup") {
    it("should append lists after serialization") {
      val l1 = List(1, 2, 3)
      val l2 = List(10, 20, 30)
      (tojson(l1) |@| tojson(l2))(_ |+| _) should equal(tojson(l1 |+| l2))
    }

    it("should append Maps after serialization") {
      val m1 = Map("1" -> "a", "2" -> "b", "3" -> "c")
      val m2 = Map("4" -> "d", "5" -> "e")
      (tojson(m1) |@| tojson(m2))(_ |+| _) should equal(tojson(m1 |+| m2))
      val m3 = Map("4" -> "d", "3" -> "e")
      (tojson(m1) |@| tojson(m3))(_ |+| _) should equal(tojson(m1 |+| m3))
    }

    it("should append Maps with objects after serialization") {
      val m1 = Map("1" -> Map("1" -> "a", "2" -> "b", "3" -> "c"))
      val m2 = Map("1" -> Map("2" -> "x"), "2" -> Map("3" -> "d", "5" -> "e"))
      (tojson(m1) |@| tojson(m2))(_ |+| _) should equal(tojson(m1 |+| m2))
    }

    it("should append objects to form larger ones") {
      val name = Map("firstName" -> "debasish", "lastName" -> "ghosh")
      val address = Map("no" -> "1050/2", "street" -> "survey park", "zip" -> "700075")
      val s1 = Map("salary" -> 1000)
      val s2 = Map("salary" -> 1200)
      val s3 = Map("salary" -> 1700)
      (tojson(name) |@| tojson(address))(_ |+| _) should equal(tojson(name |+| address))
      (tojson(s1) |@| tojson(s2) |@| tojson(s3))(_ |+| _ |+| _) should equal(tojson(s1 |+| s2 |+| s3))
    }

    it("should app objects to form larger ones") {
      val name = Map("firstName" -> "debasish", "lastName" -> "ghosh")
      val address = Map("no" -> "1050/2", "street" -> "survey park", "zip" -> "700075")
      val s1 = Map("salary" -> 1000)
      val s2 = Map("salary" -> 1200)
      val s3 = Map("salary" -> 1700)
      (tojson(name) |@| tojson(address) |@| tojson(s1) |@| tojson(s2) |@| tojson(s3))(_ |+| _ |+| _ |+| _ |+| _) should equal((tojson(name |+| address) |@| tojson(s1 |+| s2 |+| s3))(_ |+| _))
    }

    it("should append user defined objects to form larger ones") {
      case class Me(firstName: String, lastName: String, no: String, street: String, zip: String)
      implicit val MeFormat: Format[Me, JsValue] =
        asProduct5("firstName", "lastName", "no", "street", "zip")(Me)(Me.unapply(_).get)

      val name = Map("firstName" -> "debasish", "lastName" -> "ghosh")
      val address = Map("no" -> "1050/2", "street" -> "survey park", "zip" -> "700075")
      val me = Me("debasish", "ghosh", "1050/2", "survey park", "700075")
      fromjson[Me](tojson(name).toOption.get |+| tojson(address).toOption.get) should equal(me.success)
    }
  }

  describe("Composing JsObject using Applicative") {
    it("should allow composition of json as applicatives") {
      case class Me(firstName: String, lastName: String, no: String, street: String, zip: String)
      implicit val MeFormat: Format[Me, JsValue] =
        asProduct5("firstName", "lastName", "no", "street", "zip")(Me)(Me.unapply(_).get)

      val me = Me("debasish", "ghosh", "1050/2", "survey park", "700075")
      val json = tojson(me)
      val x =
        for {
          fn <- field_c[String]("firstName")
          ln <- field_c[String]("lastName")
          no <- field_c[String]("no")
          st <- field_c[String]("street")
          zp <- field_c[String]("zip")
        }
        yield(fn |@| ln |@| no |@| st |@| zp)
      x(json.toOption.get) {(f, l, n, s, z) => Me(f, l, n, s, z)} should equal(Success(me))
    }

    it("should allow accumulation of errors in composition of json as applicatives") {
      case class Me(firstName: String, lastName: String, no: String, street: String, zip: String)
      implicit val MeFormat: Format[Me, JsValue] =
        asProduct5("firstName", "lastName", "no", "street", "zip")(Me)(Me.unapply(_).get)

      val me = Me("debasish", "ghosh", "1050/2", "survey park", "700075")
      val json = tojson(me)
      val x =
        for {
          fn <- field_c[String]("firstname")
          ln <- field_c[String]("lastname")
          no <- field_c[String]("no")
          st <- field_c[String]("street")
          zp <- field_c[String]("zip")
        }
        yield(fn |@| ln |@| no |@| st |@| zp)
      val err = x(json.toOption.get) {(f, l, n, s, z) => Me(f, l, n, s, z)}
      err.fail.toOption.get.list should equal(List("field firstname not found", "field lastname not found"))
    }
  }
}
