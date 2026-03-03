// RUN: %target-resilience-test --additional-compile-flags '-enable-experimental-feature CoroutineAccessors'

// REQUIRES: executable_test
// REQUIRES: swift_feature_CoroutineAccessors

// Linux fails with the following removed symbols:
// Removed Symbols:
// $s19coroutine_accessors13ResilientEnumO1sSivg
// $s19coroutine_accessors13ResilientEnumO1sSivpMV
// $s19coroutine_accessors14ResilientClassC1sSivgTj
// $s19coroutine_accessors14ResilientClassC1sSivgTq
// $s19coroutine_accessors14ResilientClassC1sSivpMV
// $s19coroutine_accessors15ResilientStructV1sSivg
// $s19coroutine_accessors15ResilientStructV1sSivpMV
// $s19coroutine_accessors33ResilientStructWithImplicitModifyV1sSivg
// $s19coroutine_accessors33ResilientStructWithImplicitModifyV1sSivpMV
// XFAIL: OS=linux-gnu

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
