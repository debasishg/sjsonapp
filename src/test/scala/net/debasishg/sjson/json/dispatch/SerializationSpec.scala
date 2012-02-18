package net.debasishg
package sjson
package json
package dispatchprotocol

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import dispatch.json.JsValue

@RunWith(classOf[JUnitRunner])
class SerializationSpec extends GenericSerializationSpec[JsValue] {
  import rosetta.json.dispatch._
  val jsonImplementation = JsonDispatch
}
