// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

protocol Horse {
  init()
}

class Pony : NSObject, Horse {
  override required init() {}
}

class ChincoteaguePony : Pony {}

var ObjCMetatypesTest = TestSuite("ObjCMetatypes")

ObjCMetatypesTest.test("ClassInit") {
  let metatype: Pony.Type = ChincoteaguePony.self
  let instance = metatype.init()
  expectEqual(type(of: instance), ChincoteaguePony.self)
}

ObjCMetatypesTest.test("ProtocolInit") {
  let metatype: Horse.Type = ChincoteaguePony.self
  let instance = metatype.init()
  expectEqual(type(of: instance), ChincoteaguePony.self)
}

runAllTests()
