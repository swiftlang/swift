// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

@objc protocol Horse {
  init()
}

class Pony : Horse {
  let x = LifetimeTracked(0)

  required init() {}
}

var ObjCProtocolsTest = TestSuite("ObjCProtocols")

ObjCProtocolsTest.test("InitRequirement") {
  let t: Horse.Type = Pony.self

  _ = t.init()
}

@objc protocol OptionalRequirements {
  @objc optional func returnNumber(n: Int) -> Int
  @objc optional static func returnNumberStatic(n: Int) -> Int

  @objc optional func returnSelf() -> Self
}

ObjCProtocolsTest.test("OptionalMethodReferenceTypes") {
  class Base {}
  class Derived: Base, OptionalRequirements {}

  let exist: OptionalRequirements = Derived()
  let existComposition: Base & OptionalRequirements = Derived()
  let existMeta: OptionalRequirements.Type = Derived.self

  expectEqual(type(of: exist.returnNumber),
              ((Int) -> Int)?.self)
  expectEqual(type(of: existMeta.returnNumberStatic),
              ((Int) -> Int)?.self)
  expectEqual(type(of: OptionalRequirements.returnNumber),
              ((OptionalRequirements) -> ((Int) -> Int)?).self)
  expectEqual(type(of: (Base & OptionalRequirements).returnNumber),
              ((Base & OptionalRequirements) -> ((Int) -> Int)?).self)

  expectEqual(type(of: exist.returnSelf),
              (() -> OptionalRequirements)?.self)
  expectEqual(type(of: existComposition.returnSelf),
              (() -> Base & OptionalRequirements)?.self)
  expectEqual(type(of: OptionalRequirements.returnSelf),
              ((OptionalRequirements) -> (() -> OptionalRequirements)?).self)
  expectEqual(type(of: (Base & OptionalRequirements).returnSelf),
              ((Base & OptionalRequirements) -> (() -> Base & OptionalRequirements)?).self)

}

ObjCProtocolsTest.test("OptionalMethodReferencesNoImplementation") {
  class Class: OptionalRequirements {}

  let exist: OptionalRequirements = Class()
  let existMeta: OptionalRequirements.Type = Class.self

  expectNil(exist.returnNumber)
  expectNil(existMeta.returnNumberStatic)
  expectNil(OptionalRequirements.returnNumber(exist))

  expectNil(exist.returnSelf)
  expectNil(OptionalRequirements.returnSelf(exist))
}

ObjCProtocolsTest.test("OptionalMethodReferencesWithImplementation") {
  class Class: OptionalRequirements {
    func returnSelf() -> Self {
      return self
    }

    func returnNumber(n: Int) -> Int {
      return n
    }
    static func returnNumberStatic(n: Int) -> Int {
      return n
    }
  }

  let exist: OptionalRequirements = Class()
  let existMeta: OptionalRequirements.Type = Class.self

  let returnNumberRef = exist.returnNumber
  let returnNumberStaticRef = existMeta.returnNumberStatic
  let returnNumberUnboundRef = OptionalRequirements.returnNumber

  expectEqual(returnNumberRef!(123), 123)
  expectEqual(returnNumberStaticRef!(456), 456)
  expectEqual(returnNumberUnboundRef(exist)!(789), 789)

  let returnSelfRef = exist.returnSelf
  let returnSelfUnboundRef = OptionalRequirements.returnSelf

  expectEqual(returnSelfRef!().returnNumber!(n: 321), 321)
  expectEqual(returnSelfUnboundRef(exist)!().returnNumber!(n: 456), 456)
}

runAllTests()
