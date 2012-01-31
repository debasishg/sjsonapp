package net.debasishg
package sjson
package json

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import dispatch.json._
import scalaz._
import Scalaz._

@RunWith(classOf[JUnitRunner])
class SerializationSpec extends Spec with ShouldMatchers {

  import DefaultProtocol._
  import JsonSerialization._

  describe("Serialization using Person protocol") {
    it ("should serialize a Person") {
      case class Person(firstName: String, lastName: String, gender: String, age: Int)

      implicit val PersonFormat: Format[Person] = new Format[Person] {

        def reads(json: JsValue): ValidationNEL[String, Person] = json match {
          case m@JsObject(_) =>
            (field[String]("firstName", m)             |@| 
             field[String]("lastName", m)              |@| 
             field[String]("gender", m)                |@| 
             field[Int]("age", m)) { Person }

          case _ => "JsObject expected".fail.liftFailNel
        }

        def writes(p: Person) =
          List(
            tojson("firstName")      <|*|> tojson(p.firstName),
            tojson("lastName")       <|*|> tojson(p.lastName),
            tojson("gender")         <|*|> tojson(p.gender),
            tojson("age")            <|*|> tojson(p.age)
          ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
            case Success(kvs) => JsObject(kvs.map{case (key, value) => (key.asInstanceOf[JsString], value)}).success
            case Failure(errs) => errs.fail
          }
      }
      val p = Person("ghosh", "debasish", "M", 27)
      fromjson[Person](tojson(p).toOption.get) should equal(p.success)
    }

    it ("should serialize a Person and use the supplied validations") {
      case class Person(firstName: String, lastName: String, gender: String, age: Int)

      val validGender: String => ValidationNEL[String, String] = {g =>
        if (g == "M" || g == "F") g.success else "gender must be M or F".fail.liftFailNel
      }

      val validAge: Int => ValidationNEL[String, Int] = {a =>
        if (a < 0 || a > 100) "age must be positive and < 100".fail.liftFailNel else a.success
      }

      implicit val PersonFormat: Format[Person] = new Format[Person] {

        def reads(json: JsValue): ValidationNEL[String, Person] = json match {
          case m@JsObject(_) =>
            (field[String]("firstName", m)             |@| 
             field[String]("lastName", m)              |@| 
             field[String]("gender", m, validGender)   |@| 
             field[Int]("age", m, validAge)) { Person }

          case _ => "JsObject expected".fail.liftFailNel
        }

        def writes(p: Person) =
          List(
            tojson("firstName")      <|*|> tojson(p.firstName),
            tojson("lastName")       <|*|> tojson(p.lastName),
            tojson("gender")         <|*|> tojson(p.gender),
            tojson("age")            <|*|> tojson(p.age)
          ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
            case Success(kvs) => JsObject(kvs.map{case (key, value) => (key.asInstanceOf[JsString], value)}).success
            case Failure(errs) => errs.fail
          }
      }
      val p = Person("ghosh", "debasish", "M", 27)
      fromjson[Person](tojson(p).toOption.get) should equal(p.success)

      val q = Person("ghosh", "debasish", "G", 27)
      fromjson[Person](tojson(q).toOption.get).fail.toOption.get.list should equal(List("gender must be M or F"))

      val r = Person("ghosh", "debasish", "G", 270)
      fromjson[Person](tojson(r).toOption.get).fail.toOption.get.list should equal(List("gender must be M or F", "age must be positive and < 100"))
    }
  }

  describe("Serialize and then selectively pick and validate") {
    it("should serialize and validate") {
      case class Me(firstName: String, lastName: String, age: Int, no: String, street: String, zip: String)
      implicit val MeFormat: Format[Me] =
        asProduct6("firstName", "lastName", "age", "no", "street", "zip")(Me)(Me.unapply(_).get)
  
      val me = Me("debasish", "ghosh", 30, "1050/2", "survey park", "700075")
      val json = tojson(me)

      val positive: Int => ValidationNEL[String, Int] = 
        (i: Int) => if (i > 0) i.success else "must be +ve".fail.liftFailNel

      val min: Int => ValidationNEL[String, Int] = 
        (i: Int) => if (i > 10) i.success else "must be > 10".fail.liftFailNel

      val max: Int => ValidationNEL[String, Int] = 
        (i: Int) => if (i < 100) i.success else "must be < 100".fail.liftFailNel

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
}
