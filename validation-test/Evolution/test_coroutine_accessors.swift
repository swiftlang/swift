// RUN: %target-resilience-test --additional-compile-flags '-enable-experimental-feature CoroutineAccessors'

// REQUIRES: executable_test
// REQUIRES: swift_feature_CoroutineAccessors

import StdlibUnittest
import coroutine_accessors


var CoroutineAccessorsTest = TestSuite("CoroutineAccessors")

CoroutineAccessorsTest.test("ResilientStruct") {
  var r = ResilientStruct()
  expectEqual(r.s, 0)
  r.s = 5
  expectEqual(r.s, 5)
}

CoroutineAccessorsTest.test("ResilientStructWithImplicitModify") {
  var r = ResilientStruct()
  expectEqual(r.s, 0)
  r.s = 5
  expectEqual(r.s, 5)
}

CoroutineAccessorsTest.test("ResilientEnum") {
  var r = ResilientEnum.a(0)
  expectEqual(r.s, 0)
  r.s = 5
  expectEqual(r.s, 5)
}

CoroutineAccessorsTest.test("ResilientClass") {
  var r = ResilientClass()
  expectEqual(r.s, 0)
  r.s = 5
  expectEqual(r.s, 5)
}


runAllTests()
