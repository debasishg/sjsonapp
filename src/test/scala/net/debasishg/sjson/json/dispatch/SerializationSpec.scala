package net.debasishg
package sjson
package json
package dispatchprotocol

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalaz._
import Scalaz._

import dispatch.json.JsValue

@RunWith(classOf[JUnitRunner])
class SerializationSpec extends GenericSerializationSpec[JsValue] {
  import rosetta.json.dispatch._
  val jsonImplementation = JsonDispatch

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
}
