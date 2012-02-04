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

  describe("Serialize using binary") {
    import CompositeProtocol._

    it("should serialize") {
      val name = Name("debasish", "ghosh")
      frombinary[Name](tobinary(name).toOption.get) should equal(name.success)

      val address = Address("1050/2", "Survey Park", "700075")
      val me = Me(name, 40, address)

      frombinary[Me](tobinary(me).toOption.get) should equal(me.success)
    }
  }

  describe("Serialization of lists") {
    it ("should serialize list of Ints") {
      val l1 = List(100, 200, 300, 400)
      tojson(l1) map fromjson[List[Int]] should equal(l1.success.success)
    }

    it ("should serialize list of Strings") {
      val l2 = List("dg", "mc", "rc", "nd")
      tojson(l2) map fromjson[List[String]] should equal(l2.success.success)
    }
  }

  describe("Serialization of Maps") {
    it ("should serialize Map of Strings & Strings") {
      val m = Map("100" -> "dg", "200" -> "mc")
      tojson(m) map fromjson[Map[String, String]] should equal(m.success.success)
    }
  }

  describe("Serialization of composite objects with arrays") {
    import AccountProtocol._

    it("should serialize into json and back") {
      val account = Account("123", "Debasish Ghosh",
        Array(Address(100, "monroe st", "denver", "80231"), Address(234, "pine drive", "santa clara", "95054")))

      val Success(Success(ac)) = (tojson(account) map fromjson[Account])
      ac.no should equal(account.no)
      ac.name should equal(account.name)
      ac.addresses should be === account.addresses
    }
  }

  describe("Serialization of Option") {
    import AddressWithOptionalCityProtocol._

    it("should serialize an option field") {
      val str = Some("debasish")
      tojson[Option[String]](str) map fromjson[Option[String]] should equal(str.success.success)
      tojson[Option[String]](None) map fromjson[Option[String]] should equal(None.success.success)

      val i = Some(200)
      tojson[Option[Int]](i) map fromjson[Option[Int]] should equal(i.success.success)
    }
    it("should serialize AddressWithOptionalCity") {
      val ad = AddressWithOptionalCity("garer math", Some("mumbai"), "400087")
      tojson(ad) map fromjson[AddressWithOptionalCity] should equal(ad.success.success)
    }
    it("should serialize AddressWithOptionalCity without city") {
      val ad = AddressWithOptionalCity("garer math", None, "400087")
      tojson(ad) map fromjson[AddressWithOptionalCity] should equal(ad.success.success)
    }
  }

  describe("Serialization of tuples") {
    import AccountProtocol._

    it("should serialize tuples of primitive types") {
      val t1 = ("debasish", 12)
      tojson(t1) map fromjson[Tuple2[String, Int]] should equal(t1.success.success)
      val t2 = ("debasish", 12, "jonas")
      tojson(t2) map fromjson[Tuple3[String, Int, String]] should equal(t2.success.success)
    }
    it("should serialize tuples of user defined types") {
      val t1 = ("debasish", Address(102, "monroe st", "denver", "80231"))
      tojson[Tuple2[String, Address]](t1) map fromjson[Tuple2[String, Address]] should equal(t1.success.success)
    }
  }

  describe("Serialization of complex types") {
    it("should serialize complex types") {
      val l = List(Map("1"->"dg", "2"->"mc"), Map("1"->"irc", "2"->"rc", "3"->"nd"))
      tojson(l) map fromjson[List[Map[String, String]]] should equal(l.success.success)
    }
  }

  describe("Serialization of wrappers") {
    import WrapperProtocol._

    it("should serialize") {
      val n = Name("debasish ghosh")
      tojson(n) map fromjson[Name] should equal(n.success.success)
    }
    it("should serialize list wrappers") {
      val n = Holder(List("debasish ghosh", "jonas boner", "stephan schmidt"))
      tojson(n) map fromjson[Holder] should equal(n.success.success)
    }
  }

  describe("Serialization with inheritance") {
    import InheritanceProtocol._

    it("should serialize") {
      val sa = new Derived("123", "debasish ghosh", List(Address(100, "monroe st", "denver", "80231"), Address(23, "tamarac st", "boulder", "80231")), true)
      val Success(acc) = tojson(sa) map fromjson[Derived]
      acc should equal(sa.success)
    }
  }

  describe("Serialization with case objects") {
    import CaseObjectProtocol._

    it("should serialize") {
      val h = Http("http://www.google.com", Get)
      val h1 = tojson(h) map fromjson[Http]
      h1 should equal(h.success.success)
    }
  }

  describe("Serialization of mutually recursive types") {
    import RecursiveProtocol._

    it("should serialize without recursion") {
      val f = Foo("testFoo", List(Bar("test1", None), Bar("test2", None)))
      tojson(f) map fromjson[Foo] should equal(f.success.success)
    }
    it("should serialize with recursion") {
      val fBar = Foo("testFoo", List(Bar("test1", Some(List(Foo("barList", List(Bar("test", None), Bar("test2", None))))))))
      tojson(fBar) map fromjson[Foo] should equal(fBar.success.success)
    }
  }
}
