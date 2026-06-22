// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
// REQUIRES: executable_test

import EmptyFieldLayout
import StdlibUnittest

var Tests = TestSuite("EmptyFieldLayout")

Tests.test("empty before int") {
  let v = makeEmptyThenInt(42)
  expectEqual(42, v.i)
}

Tests.test("int before empty") {
  let v = makeIntThenEmpty(7)
  expectEqual(7, v.i)
}

Tests.test("two empties then int") {
  let v = makeEmptyEmptyInt(99)
  expectEqual(99, v.i)
}

Tests.test("int empty int") {
  let v = makeIntEmptyInt(11, 22)
  expectEqual(11, v.a)
  expectEqual(22, v.b)
}

Tests.test("derived from empty base") {
  let v = makeDerivedFromEmpty(123)
  expectEqual(123, v.i)
}

Tests.test("no_unique_address empty") {
  let v1 = makeNoUniqueAddressEmpty(77)
  expectEqual(77, v1.i)

  let v2 = makeIntTrailingNoUniqueAddressEmpty(88)
  expectEqual(88, v2.i)
}

Tests.test("int empty int int (16 bytes, mid empty)") {
  let v = makeIntEmptyIntInt(11, 22, 33)
  expectEqual(11, v.a)
  expectEqual(22, v.b)
  expectEqual(33, v.c)
}

Tests.test("non-trivial dtor (address-only, sret path)") {
  let v = makeEmptyAndIntNonTrivial(2026)
  expectEqual(2026, v.i)
}

runAllTests()

