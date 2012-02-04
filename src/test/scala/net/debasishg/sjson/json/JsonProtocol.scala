package net.debasishg
package sjson
package json

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalaz._
import Scalaz._
import rosetta.json._

trait JsonProtocol[Json] extends DefaultProtocol[Json] {self: JsonSerialization[Json] =>
  val jsonImplementation: JsonImplementation[Json]
  import jsonImplementation._

  object PersonProtocol {
    case class Person(firstName: String, lastName: String, gender: String, age: Int)

    implicit val PersonFormat: Format[Person, Json] = new Format[Person, Json] {

      def reads(json: Json): ValidationNEL[String, Person] = json match {
        case m@JsonObject(_) =>
          (field[String]("firstName", m)             |@| 
           field[String]("lastName", m)              |@| 
           field[String]("gender", m)                |@| 
           field[Int]("age", m)) { Person }

        case _ => "JsonObject expected".fail.liftFailNel
      }

      def writes(p: Person) =
        List(
          "firstName".success      <|*|> tojson(p.firstName),
          "lastName".success       <|*|> tojson(p.lastName),
          "gender".success         <|*|> tojson(p.gender),
          "age".success            <|*|> tojson(p.age)
        ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
          case Success(kvs) => JsonObject(kvs.map{case (key, value) => (key.asInstanceOf[String], value)}).success
          case Failure(errs) => errs.fail
        }
    }

    val validGender: String => ValidationNEL[String, String] = {g =>
      if (g == "M" || g == "F") g.success else "gender must be M or F".fail.liftFailNel
    }

    val validAge: Int => ValidationNEL[String, Int] = {a =>
      if (a < 0 || a > 100) "age must be positive and < 100".fail.liftFailNel else a.success
    }

    case class VPerson(firstName: String, lastName: String, gender: String, age: Int)

    implicit val VPersonFormat: Format[VPerson, Json] = new Format[VPerson, Json] {

      def reads(json: Json): ValidationNEL[String, VPerson] = json match {
        case m@JsonObject(_) =>
          (field[String]("firstName", m)             |@| 
           field[String]("lastName", m)              |@| 
           field[String]("gender", m, validGender)   |@| 
           field[Int]("age", m, validAge)) { VPerson }

        case _ => "JsonObject expected".fail.liftFailNel
      }

      def writes(p: VPerson) =
        List(
          "firstName".success      <|*|> tojson(p.firstName),
          "lastName".success       <|*|> tojson(p.lastName),
          "gender".success         <|*|> tojson(p.gender),
          "age".success            <|*|> tojson(p.age)
        ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
          case Success(kvs) => JsonObject(kvs.map{case (key, value) => (key.asInstanceOf[String], value)}).success
          case Failure(errs) => errs.fail
        }
    }
  }

  object MeProtocol {
    case class Me(firstName: String, lastName: String, age: Int, no: String, street: String, zip: String)
    implicit val MeFormat: Format[Me, Json] =
      asProduct6("firstName", "lastName", "age", "no", "street", "zip")(Me)(Me.unapply(_).get)
  
    val positive: Int => ValidationNEL[String, Int] = 
      (i: Int) => if (i > 0) i.success else "must be +ve".fail.liftFailNel

    val min: Int => ValidationNEL[String, Int] = 
      (i: Int) => if (i > 10) i.success else "must be > 10".fail.liftFailNel
  
    val max: Int => ValidationNEL[String, Int] = 
      (i: Int) => if (i < 100) i.success else "must be < 100".fail.liftFailNel
  }

  object CompositeProtocol {
    case class Address(no: String, street: String, zip: String)
    implicit val AddressFormat: Format[Address, Json] = 
      asProduct3("no", "street", "zip")(Address)(Address.unapply(_).get)

    case class Name(firstName: String, lastName: String)
    implicit val NameFormat: Format[Name, Json] =
      asProduct2("firstName", "lastName")(Name)(Name.unapply(_).get)

    case class Me(name: Name, age: Int, address: Address)
    implicit val MeFormat: Format[Me, Json] =
      asProduct3("name", "age", "address")(Me)(Me.unapply(_).get)
  }

  object AccountProtocol {
    case class Address(no: Int, street: String, city: String, zip: String)
    implicit val AddressFormat: Format[Address, Json] = 
      asProduct4("no", "street", "city", "zip")(Address)(Address.unapply(_).get)

    case class Account(no: String, name: String, addresses: Array[Address])
    implicit val AccountFormat: Format[Account, Json] =
      asProduct3("no", "name", "addresses")(Account)(Account.unapply(_).get)
  }

  object AddressWithOptionalCityProtocol {
    case class AddressWithOptionalCity(street: String, city: Option[String], zip: String)
    implicit val AddressWithOptionalCityFormat: Format[AddressWithOptionalCity, Json] =
      asProduct3("street", "city", "zip")(AddressWithOptionalCity)(AddressWithOptionalCity.unapply(_).get)
  }

  object WrapperProtocol {
    case class Name(name: String)
    implicit val NameFormat: Format[Name, Json] = wrap[Name, String]("name")(_.name, Name)

    case class Holder(item: List[String])
    implicit val HolderFormat: Format[Holder, Json] = wrap[Holder, List[String]]("item")(_.item, Holder)
  }

  object InheritanceProtocol {
    case class Address(no: Int, street: String, city: String, zip: String)
    implicit val AddressFormat: Format[Address, Json] = 
      asProduct4("no", "street", "city", "zip")(Address)(Address.unapply(_).get)

    case class Base(no: String, name: String, addresses: List[Address])
    implicit val BaseFormat: Format[Base, Json] =
      asProduct3("no", "name", "addresses")(Base)(Base.unapply(_).get)

    class Derived(no: String, name: String, addresses: List[Address], special: Boolean)
      extends Base(no, name, addresses) {
      val specialFlag = special
    }
    implicit val DerivedFormat: Format[Derived, Json] = new Format[Derived, Json] {
      def reads(json: Json): Validation[NonEmptyList[String], Derived] = {
        val Success(b) = fromjson[Base](json)
        json match {
          case m@JsonObject(_) =>
            val Success(spl) = field[Boolean]("specialFlag", m)
            new Derived(b.no, b.name, b.addresses, spl).success
          case _ => "JsonObject expected".fail.liftFailNel
        }
      }
      def writes(a: Derived) = {
        val Success(o) = tojson(a: Base)
        val JsonObject(m) = o
        JsonObject(m ++ List(("specialFlag", tojson(a.specialFlag).toOption.get))).success
      }
    }
  }

  object CaseObjectProtocol {
    sealed trait HttpType
    implicit val HttpTypeFormat: Format[HttpType, Json] = new Format[HttpType, Json] {
      def reads(json: Json): Validation[NonEmptyList[String], HttpType] = json match {
        case JsonString("Get") => Get.success
        case JsonString("Post") => Post.success
        case _ => "Invalid HttpType".fail.liftFailNel
      }
      def writes(a: HttpType) = a match {
        case Get => JsonString("Get").success
        case Post => JsonString("Post").success
      }
    }

    case object Get extends HttpType
    case object Post extends HttpType

    case class Http(url: String, t: HttpType)
    implicit val HttpFormat: Format[Http, Json] =
      asProduct2("url", "t")(Http)(Http.unapply(_).get)
  }

  object RecursiveProtocol {
    case class Bar(name: String, list: Option[List[Foo]])
    case class Foo(name: String, list: List[Bar])
    implicit val BarFormat: Format[Bar, Json] = lazyFormat(asProduct2("name", "list")(Bar)(Bar.unapply(_).get))
    implicit val FooFormat: Format[Foo, Json] = lazyFormat(asProduct2("name", "list")(Foo)(Foo.unapply(_).get))
  }
}
