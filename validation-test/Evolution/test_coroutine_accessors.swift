// RUN: %target-resilience-test-coroutine-accessors

// The symbol diffing code does not seem to work on linux:
//  nm: invalid argument to -U/--unicode
// AssertionError: ['nm', '-gjU', '/home/../Output/test_coroutine_accessors.swift.tmp/before/libcoroutine_accessors.so'
// UNSUPPORTED: OS=linux-gnu

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
