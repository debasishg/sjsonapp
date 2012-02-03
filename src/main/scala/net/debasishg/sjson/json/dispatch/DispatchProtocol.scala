package net.debasishg
package sjson.json
package dispatchprotocol

import dispatch.json._

object DispatchJsonSerialization extends JsonSerialization[JsValue] with DefaultProtocol[JsValue] {
  import rosetta.json.dispatch._
  val jsonImplementation = JsonDispatch
}
