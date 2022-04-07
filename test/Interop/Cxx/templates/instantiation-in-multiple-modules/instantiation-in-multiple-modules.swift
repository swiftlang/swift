// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import A
import B
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("instantiation-in-multiple-modules") {
  var w = WrapperInt()
  // Make sure that WrapperInt::foo gets called.
  expectEqual(w.i, 1)
}

runAllTests()
