// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

// RUN: %target-build-swift %s 2> %t.warnings.txt
// RUN: FileCheck -check-prefix=CHECK-WARNINGS %s < %t.warnings.txt

import StdlibUnittest

import Foundation

var tests = TestSuite("NSError")

tests.test("user info") {
  let error = NSError(domain: "MyDomain", code: 1, userInfo: [
      // CHECK-WARNINGS: warning: 'localizedDescriptionKey' is deprecated: renamed to 'NSLocalizedDescriptionKey'
      // CHECK-WARNINGS: note: use 'NSLocalizedDescriptionKey' instead
        ErrorUserInfoKey.localizedDescriptionKey.rawValue as NSString: "description",
        NSLocalizedFailureReasonErrorKey as NSString: "reason"
      ])
  expectEqual("description", error.userInfo[NSLocalizedDescriptionKey as NSString]! as! String)
  expectEqual("reason", error.userInfo[ErrorUserInfoKey.localizedFailureReasonErrorKey.rawValue as NSObject]! as! String)

  expectEqual("reason", error.userInfo[ErrorUserInfoKey.localizedFailureReasonErrorKey as NSObject]! as! String)
}

runAllTests()
