package net.debasishg
package sjson
package json
package blueeyesprotocol

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import blueeyes.json.JsonAST._

import rosetta.json._
import rosetta.json.blueeyes._

@RunWith(classOf[JUnitRunner])
class SerializationSpec extends GenericSerializationSpec[JValue] {
  val jsonImplementation = JsonBlueEyes
}
