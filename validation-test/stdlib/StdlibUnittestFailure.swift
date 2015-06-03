// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out
// REQUIRES: executable_test
// XFAIL: *

//
// Check that failures coming from StdlibUnittest are counted as failures by lit.
//

import StdlibUnittest

var TestSuiteFails = TestSuite("TestSuiteFails")

TestSuiteFails.test("fails") {
  expectEqual(1, 2)
}

runAllTests()

