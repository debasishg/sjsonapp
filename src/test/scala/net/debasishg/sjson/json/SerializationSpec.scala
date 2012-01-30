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
}

