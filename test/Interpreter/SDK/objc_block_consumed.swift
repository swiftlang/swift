// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/objc_block_consumed.h -o %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

class C : NSObject {
  var tracked = LifetimeTracked(0)
}

var ObjCBlockConsumedTestSuite = TestSuite("ObjCBlockConsumed")

ObjCBlockConsumedTestSuite.test("Test") {
  takesBlockWithConsumedArg({ arg in }, C())
}

runAllTests()
