// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: not --crash %target-run %t.out | FileCheck %s

// FIXME: this check doesn't work in the iOS simulator or device environment.
// XFAIL: OS=ios

//
// Check that failures coming from StdlibUnittest are counted as failures by lit.
//

import StdlibUnittest

var TestSuitePasses = TestSuite("TestSuitePasses")

TestSuitePasses.test("passes") {
  expectEqual(1, 1)
}

runAllTests()
runAllTests()
// CHECK: runAllTests() called twice.  It is not allowed, aborting.

