// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import EmitCalledConstructor
import StdlibUnittest

var MembersTestSuite = TestSuite("MembersTestSuite")

MembersTestSuite.test("transitive-function-constructor") {
  expectEqual(42, useIncrementor())
}

runAllTests()