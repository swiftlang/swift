// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/static-var.cpp -I %S/Inputs -o %t/static-var.o -std=c++17
// RUN: %target-build-swift %s -I %S/Inputs -o %t/statics %t/static-var.o -Xfrontend -enable-cxx-interop
// RUN: %target-codesign %t/statics
// RUN: %target-run %t/statics
//
// REQUIRES: executable_test

import StaticVar
import StdlibUnittest

var StaticVarTestSuite = TestSuite("StaticVarTestSuite")

StaticVarTestSuite.test("static-int") {
  expectEqual(2, staticVar)
}

StaticVarTestSuite.test("static-int-write-from-swift") {
  expectNotEqual(3, staticVar)
  staticVar = 3
  expectEqual(3, getStaticVarFromCxx())
}

StaticVarTestSuite.test("static-int") {
  expectNotEqual(4, staticVar)
  setStaticVarFromCxx(4)
  expectEqual(4, staticVar)
}

StaticVarTestSuite.test("static-int-inline-init") {
  expectEqual(8, staticVarInlineInit)
}

StaticVarTestSuite.test("static-int-init") {
  expectEqual(64, staticVarInit)
}

StaticVarTestSuite.test("static-const-int") {
  expectEqual(4, staticConst)
}

StaticVarTestSuite.test("static-const-int-inline-init") {
  expectEqual(16, staticConstInlineInit)
}

StaticVarTestSuite.test("static-const-int-init") {
  expectEqual(128, staticConstInit)
}

StaticVarTestSuite.test("static-constexpr-int") {
  expectEqual(32, staticConstexpr)
}

StaticVarTestSuite.test("static-non-trivial") {
  expectEqual(1024, staticNonTrivial.val)
}

StaticVarTestSuite.test("static-non-trivial-address") {
  expectEqual(getstaticNonTrivialFromCxx(), &staticNonTrivial)
}

StaticVarTestSuite.test("static-non-trivial-write-from-cxx") {
  expectNotEqual(1025, staticNonTrivial.val)
  setstaticNonTrivialFromCxx(1025)
  expectEqual(1025, staticNonTrivial.val)
}

StaticVarTestSuite.test("static-non-trivial-write-from-swift") {
  expectNotEqual(1026, staticNonTrivial.val)
  //TODO: Delete `NonTrivial()` adn use `NonTrivial(int)` constructor once
  // apple/swift/pull/30630 is merged.
  staticNonTrivial = NonTrivial()
  staticNonTrivial.val = 1026
  expectEqual(1026, getstaticNonTrivialFromCxx().pointee.val)
}

StaticVarTestSuite.test("static-const-non-trivial") {
  expectEqual(2048, staticConstNonTrivial.val)
}

StaticVarTestSuite.test("static-constexpr-non-trivial") {
  expectEqual(8192, staticConstexprNonTrivial.val)
}

runAllTests()
