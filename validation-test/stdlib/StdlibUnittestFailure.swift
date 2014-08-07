// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out
// XFAIL: *

//
// Check that failures coming from StdlibUnittest are counted as failures by lit.
//

import StdlibUnittest

var TestCaseFails = TestCase("TestCaseFails")

TestCaseFails.test("fails") {
  expectEqual(1, 2)
}

runAllTests()

