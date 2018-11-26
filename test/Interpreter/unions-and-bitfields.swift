// RUN: %target-build-swift %s -import-objc-header %S/Inputs/unions-and-bitfields.h -disable-bridging-pch -o %t
// RUN: %target-codesign %t
// RUN: %target-run %t
// REQUIRES: executable_test

// The -disable-bridging-pch above isn't actually relevant to the test; however,
// precompiled headers don't play nice with the way we include the platform
// module map on non-Apple platforms. See 
// https://bugs.llvm.org/show_bug.cgi?id=36245.

import StdlibUnittest

var suite = TestSuite("UnionsAndBitfields")

suite.test("PlainUnion") {
  var x = PlainUnion()
  populate(&x)
  expectEqual(0x11223344, x.whole)
  expectTrue(x.first == 0x11 || x.first == 0x44)
}

suite.test("PlainBitfield") {
  var x = PlainBitfield()
  populateAtOffset(&x)
  expectTrue(x.first == 0x11 || x.first == 0x44)
}

suite.test("PlainIndirect") {
  var x = PlainIndirect()
  populateAtOffset(&x)
  expectEqual(0x11223344, x.whole)
}

suite.test("BitfieldUnion") {
  var x = BitfieldUnion()
  populate(&x)
  expectEqual(0x11223344, x.whole)
  expectTrue(x.first == 0x11 || x.first == 0x44)
}

suite.test("BitfieldIndirect") {
  var x = BitfieldIndirect()
  populateAtOffset(&x)
  expectTrue(x.first == 0x11 || x.first == 0x44)
}

suite.test("UnionIndirect") {
  var x = UnionIndirect()
  populateAtOffset(&x)
  expectEqual(0x11223344, x.whole)
  expectTrue(x.first == 0x11 || x.first == 0x44)
}

suite.test("BitfieldUnionIndirect") {
  var x = BitfieldUnionIndirect()
  populateAtOffset(&x)
  expectEqual(0x11223344, x.whole)
  expectTrue(x.first == 0x11 || x.first == 0x44)
}

runAllTests()
