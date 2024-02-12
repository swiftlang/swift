// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import UnevaluatedContext

var UnevaluatedTestSuite = TestSuite("UnevaluatedContext")

UnevaluatedTestSuite.test("declval") {
  initVector()
  let _ = UseDeclValStruct().callMethod()
}

runAllTests()
