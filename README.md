
<a HREF="https://github.com/debasishg/sjson">sjson</A> has just gone applicative. I have changed the typeclasses for reading and writing jsons so that the typeclass protocols now return applicatives instead of raw types. Of course this makes the protocols composable with any other applicative based API in the world. This is the advantage of programming with generic abstractions like functors, applicatives and monads - you can compose them readily with any API that the world has written using the same ones.

Previously the serialization typeclasses for sjson looked like ..

```scala
// takes a type and produces a Json abstraction
trait Writes[T] {
  def writes(o: T): JsValue
}

// reads a json abstraction and produces T
trait Reads[T] {
  def reads(json: JsValue): T
}
```

You can compose `writes` and `reads` together only through function composition since they have symmetric type signatures. But you cannot get the benefits of threading additional effectful computations through them. e.g. I cannot accumulate errors generically in `reads` that result from mismatches in the field names between the json structure and the Scala type. Or I cannot plugin additional validations on Json structures during de-serialization into Scala type and have the validation messages passed on to the client. I need to have specialized handling in my code base by resorting to side-effects like throwing exceptions, which don't compose well.

In <a HREF="https://github.com/debasishg/sjsonapp">sjsonapp</A>, the typeclasses are changed to ..

```scala
trait Writes[T] {
  def writes(o: T): ValidationNEL[String, JsValue]
}

trait Reads[T] {
  def reads(json: JsValue): ValidationNEL[String, T]
}
```

<a HREF="https://github.com/scalaz/scalaz">scalaz</A> defines an applicative functor named `Validation` that allows you to compose validating abstractions. I had discussed in detail how you can compose domain models using applicative functors like `Validation` in an earlier <a HREF="http://debasishg.blogspot.in/2010/12/composable-domain-models-using-scalaz.html">post</A>. You can use `Validation` to accumulate errors that occur when you de-serialize a Json structure into a Scala object.

<b>Applicative Composition FTW</B>

Here's the immediate impact of making your APIs return an applicative - your json processing becomes a composable pipeline of abstractions. Here's an example of an identity operation where you serialize Scala objects into json and de-serialize them back into the same abstractions using applicative composition ..

```scala
describe("Serialize and compose applicatives") {
  it("should compose and form a bigger ADT") {
    case class Address(no: String, street: String, zip: String)
    implicit val AddressFormat: Format[Address] =
      asProduct3("no", "street", "zip")(Address)(Address.unapply(_).get)

    case class Name(firstName: String, lastName: String)
    implicit val NameFormat: Format[Name] =
      asProduct2("firstName", "lastName")(Name)(Name.unapply(_).get)

    case class Me(name: Name, age: Int, address: Address)
    implicit val MeFormat: Format[Me] =
      asProduct3("name", "age", "address")(Me)(Me.unapply(_).get)

    val name = Name("debasish", "ghosh")
    val address = Address("1050/2", "Survey Park", "700075")
    val me = Me(name, 40, address)

    fromjson[Me](tojson(me).toOption.get) should equal(me.success)

    (tojson(name) |@| tojson(address) |@| tojson(40)) {(nm, add, age) =>
      (fromjson[Name](nm) |@| fromjson[Address](add) |@| fromjson[Int](age)) {(n, ad, ag) => Me(n, ag, ad)}
    } should equal(Success(Success(me)))
  }
}
```

<b>Accumulating Validation Errors</B>

Here's a test snippet that demonstrates how you can get back validation errors in a List when de-serializing a Json structure that's supposed to make a Scala object ..

```scala
case class Person(firstName: String, lastName: String, gender: String, age: Int)
implicit val PersonFormat: Format[Person] =
  asProduct4("firstName", "lastName", "gender", "age")(Person)(Person.unapply(_).get)

val pjson = """{"FirstName" : "Debasish", "LastName" : "Ghosh", "gender": "M", "age": 40}"""
fromjson[Person](Js(pjson)).fail.toOption.get.list 
  should equal(List("field firstName not found", "field lastName not found"))
```

The power of composition with applicatives .. and note we don't use any side-effecting operations like throwing exceptions which eschews purity of your functions. The accumulation is powered by a `Semigroup` abstraction that constrains the error part of the `Validation`. Have a look at how the applicative for `Validation` constrains `X` to be a `Semigroup` in scalaz and accumulates the failures in the last clause of the pattern match ..

```scala
implicit def ValidationApply[X: Semigroup]: Apply[({type &lambda;[&alpha;]=Validation[X, &alpha;]})#&lambda;] = 
  new Apply[({type &lambda;[&alpha;]=Validation[X, &alpha;]})#&lambda;] {
    def apply[A, B](f: Validation[X, A => B], a: Validation[X, A]) = (f, a) match {
      case (Success(f), Success(a)) => success(f(a))
      case (Success(_), Failure(e)) => failure(e)
      case (Failure(e), Success(_)) => failure(e)
      case (Failure(e1), Failure(e2)) => failure(e1 |+| e2)
    }
}
```

Besides mismatches in field names, you can also plug in custom validation functions that will be invoked during de-serialization of your json structures and report similar errors when they fail .. Here's an example ..

```scala
case class Person(firstName: String, lastName: String, gender: String, age: Int)

val validGender: String => ValidationNEL[String, String] = {g =>
  if (g == "M" || g == "F") g.success else "gender must be M or F".fail.liftFailNel
}

val validAge: Int => ValidationNEL[String, Int] = {a =>
  if (a < 0 || a > 100) "age must be positive and < 100".fail.liftFailNel else a.success
}

// the typeclass implementation for Person
implicit val PersonFormat: Format[Person] = new Format[Person] {

  // the de-serializing function
  def reads(json: JsValue): ValidationNEL[String, Person] = json match {
    case m@JsObject(_) =>
      (field[String]("firstName", m)            |@|
      field[String]("lastName", m)              |@|
      field[String]("gender", m, validGender)   |@| // validation plugin
      field[Int]("age", m, validAge)) { Person }
  
    case _ => "JsObject expected".fail.liftFailNel
  }
  
  // the serializing function 
  //..
}
```

Note how we plug in the validations for `gender` and `age` the typeclass instance for `Person`. Also note that these functions also return an instance of scalaz `Validation` which can be nicely composed with the return type of the reads function after constructing the instance of the `Person` class. Here's an example ..

```scala
val p = Person("ghosh", "debasish", "M", 27)
fromjson[Person](tojson(p).toOption.get) should equal(p.success)

val r = Person("ghosh", "debasish", "G", 270)
fromjson[Person](tojson(r).toOption.get).fail.toOption.get.list should 
  equal(List("gender must be M or F", "age must be positive and < 100"))
```

<b>Applicatives open up a world of possibilities</B>


Once you have your APIs based on applicatives you can use all other abstractions that applicative functors offer - e.g. you can compose validations using Kleislis ..

```scala
describe("Serialize and chain validate using Kleisli") {
  case class Me(firstName: String, lastName: String, age: Int, no: String, street: String, zip: String)
  implicit val MeFormat: Format[Me] =
    asProduct6("firstName", "lastName", "age", "no", "street", "zip")(Me)(Me.unapply(_).get)

  val positive: Int => ValidationNEL[String, Int] =
    (i: Int) => if (i > 0) i.success else "must be +ve".fail.liftFailNel

  val min: Int => ValidationNEL[String, Int] =
    (i: Int) => if (i > 10) i.success else "must be > 10".fail.liftFailNel

  val max: Int => ValidationNEL[String, Int] =
    (i: Int) => if (i < 100) i.success else "must be < 100".fail.liftFailNel

  it("should serialize and validate") {
    val me = Me("debasish", "ghosh", 30, "1050/2", "survey park", "700075")
    val json = tojson(me)

    import Validation.Monad._
    type VA[A] = ValidationNEL[String, A]

    field[Int]("age", json.toOption.get,
      kleisli[VA, Int, Int](positive) >=> 
           kleisli[VA, Int, Int](min) >=> kleisli[VA, Int, Int](max)) should equal(30.success)

    val me1 = me.copy(age = 300)
    val json1 = tojson(me1)

    field[Int]("age", json1.toOption.get,
      kleisli[VA, Int, Int](positive) >=> 
           kleisli[VA, Int, Int](min) >=> kleisli[VA, Int, Int](max)).fail.toOption.get.list 
           should equal(List("must be < 100"))
  }
}
```
The new version of sjson with all the applicative based APIs is available in a separate repository <a href="https://github.com/debasishg/sjsonapp">sjsonapp</a> on my github. Another interesting development that will soon be available is pluggable backend for sjson. The current version (branch master) uses dispatch as the json processing backend. I am working towards making sjsonapp compatible with <a HREF="https://github.com/jdegoes/RosettaJson">RosettaJson</A>, so that you can use pluggable json processing backends like <a HREF="https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/">LiftJson</A>, <a HREF="http://dispatch.databinder.net/Dispatch.html">Dispatch</A> or <a HREF="https://github.com/jdegoes/blueeyes">BlueEyes</A>. The current RosettaJson compatible version is available in the branch <a HREF="https://github.com/debasishg/sjsonapp/tree/rosetta">rosetta</A> - feel free to checkout and play with it.

