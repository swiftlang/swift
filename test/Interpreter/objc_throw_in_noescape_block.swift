// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCException.m -c -o %t/ObjCException.o
// RUN: %target-build-swift -import-objc-header %S/Inputs/ObjCException.h -Xlinker %t/ObjCException.o %s -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var ThrowingTestSuite = TestSuite("Throwing")


ThrowingTestSuite.test("noescape verification") {
  let catcher = ExceptionCatcher()
  let e = catcher.tryBlock {
    NSException(name: NSExceptionName(rawValue: "Flames"), reason: "Fire", userInfo: nil).raise()
  }
  expectEqual(e!.reason, "Fire")
}


runAllTests()
