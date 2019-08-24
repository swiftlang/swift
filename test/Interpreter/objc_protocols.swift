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

runAllTests()
