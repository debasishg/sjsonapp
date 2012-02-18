package net.debasishg
package sjson
package json
package liftprotocol

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import net.liftweb.json.JsonAST._
import rosetta.json.lift._

@RunWith(classOf[JUnitRunner])
class SerializationSpec extends GenericSerializationSpec[JValue] {
  val jsonImplementation = JsonLift
}
