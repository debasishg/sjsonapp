package net.debasishg
package sjson
package json

import scalaz._
import Scalaz._

trait Generic[Json] extends Protocol[Json] {self: JsonSerialization[Json] =>

  import jsonImplementation._

  implicit def listFormat[T](implicit fmt: Format[T, Json]): Format[List[T], Json];

  /**
   * Use this when you would wrap a value in a case class
   *
   * <pre>
   * case class Name(name: String)
   * implicit val NameFormat: Format[Name] = wrap[Name, String]("name")(_.name, Name)
   *
   * val n = Name("debasish ghosh")
   * fromjson[Name](tojson(n)) should equal(n)
   * </pre>
   */
  def wrap[S, T](name: String)(to : S => T, from : T => S)(implicit fmt : Format[T, Json]) = new Format[S, Json]{
    def writes(s : S) = (name.success <|*|> tojson(to(s))) match {
      case Success((k: String, v)) => JsonObject((k, v)).success
      case Failure(errs) => errs.fail
    }
    def reads(js : Json) = js match {
      case JsonObject(_) =>
        val f = field[T](name, js)
        f match {
          case Success(v) => from(v).success
          case Failure(e) => e.fail
        }
      case _ => "Object expected".fail.liftFailNel
    }
  }

  /**
   * Lazy wrapper around serialization. Useful when you want to serialize mutually recursive structures.
   */
  def lazyFormat[S](fmt : => Format[S, Json]) = new Format[S, Json]{
    lazy val delegate = fmt;

    def reads(js : Json) = delegate.reads(js);
    def writes(s : S) = delegate.writes(s);
  }

<#list 2..8 as i>
<#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>

  def asProduct${i}[S, ${typeParams}](<#list 1..i as j>f${j}: String<#if i != j>,</#if></#list>)(apply : (${typeParams}) => S)(unapply : S => Product${i}[${typeParams}])(implicit <#list 1..i as j>bin${j}: Format[T${j}, Json]<#if i != j>,</#if></#list>) = new Format[S, Json]{

    def writes(s: S) = {
      val product = unapply(s)
      List(
          <#list 1..i as j>
          f${j}.success <|*|> tojson(product._${j})<#if i != j>,</#if>
          </#list>
      ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (String, Json)] match {
        case Success(kvs) => 
          JsonObject(Map.empty[String, Json] ++ kvs.map{case (k, v) => (k.asInstanceOf[String], v)}).success
        case Failure(errs) => errs.fail
      }
    }
    def reads(js: Json) = js match {
      case m@JsonObject(_) => // m is the Map
        (
          <#list 1..i as j>
          field[T${j}](f${j}, m)<#if i != j> |@|</#if>
          </#list>
        ) {apply}
      case _ => "object expected".fail.liftFailNel
    }
  }  
  </#list>
}
