package net.debasishg
package sjson
package json
package dispatchprotocol

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalaz._
import Scalaz._
import dispatch.json._

object DispatchJsonProtocol extends JsonProtocol[JsValue] with JsonSerialization[JsValue] {
  import rosetta.json.dispatch._
  val jsonImplementation = JsonDispatch
}
import DispatchJsonProtocol._

@RunWith(classOf[JUnitRunner])
class SerializationSpec extends Spec with ShouldMatchers {
  describe("Serialization using Person protocol") {
    import PersonProtocol._

    it ("should serialize a Person") {
      val p = Person("ghosh", "debasish", "M", 27)
      fromjson[Person](tojson(p).toOption.get) should equal(p.success)
    }
    it ("should serialize a Person and use the supplied validations") {
      val p = VPerson("ghosh", "debasish", "M", 27)
      fromjson[VPerson](tojson(p).toOption.get) should equal(p.success)

      val q = VPerson("ghosh", "debasish", "G", 27)
      fromjson[VPerson](tojson(q).toOption.get).fail.toOption.get.list should equal(List("gender must be M or F"))

      val r = VPerson("ghosh", "debasish", "G", 270)
      fromjson[VPerson](tojson(r).toOption.get).fail.toOption.get.list should equal(List("gender must be M or F", "age must be positive and < 100"))
    }
  }

  describe("Serialize and chain validate using Kleisli") {
    import MeProtocol._

    it("should serialize and validate") {
      val me = Me("debasish", "ghosh", 30, "1050/2", "survey park", "700075")
      val json = tojson(me)

      import Validation.Monad._
      type VA[A] = ValidationNEL[String, A]

      field[Int]("age", json.toOption.get, 
        kleisli[VA, Int, Int](positive) >=> kleisli[VA, Int, Int](min) >=> kleisli[VA, Int, Int](max)) should equal(30.success)

      val me1 = me.copy(age = 300)
      val json1 = tojson(me1)

      field[Int]("age", json1.toOption.get, 
        kleisli[VA, Int, Int](positive) >=> kleisli[VA, Int, Int](min) >=> kleisli[VA, Int, Int](max)).fail.toOption.get.list should equal(List("must be < 100"))
    }
  }

  describe("Serialize and compose applicatives") {
    import CompositeProtocol._

    it("should compose and form a bigger ADT") {
      val name = Name("debasish", "ghosh")
      val address = Address("1050/2", "Survey Park", "700075")
      val me = Me(name, 40, address)

      fromjson[Me](tojson(me).toOption.get) should equal(me.success)

      (tojson(name) |@| tojson(address) |@| tojson(40)) {(nm, add, age) => 
        (fromjson[Name](nm) |@| fromjson[Address](add) |@| fromjson[Int](age)) {(n, ad, ag) => Me(n, ag, ad)}
      } should equal(Success(Success(me)))
    }
  }

  describe("Serialize and mutate") {
    import CompositeProtocol._

    it("should serialize and then mutate some fields before de-serialize") {
      val name = Name("debasish", "ghosh")
      val json = tojson(name)
      val fields =
        for {
          f <- field_c[String]("firstName")
          l <- field_c[String]("lastName")
        } yield(f |@| l)
      fields(json.toOption.get) {(f, l) => Name(f.toUpperCase, l.toUpperCase)} should equal(Name("DEBASISH", "GHOSH").success)
    }
  }
}
