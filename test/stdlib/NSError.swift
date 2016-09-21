// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

// RUN: %target-build-swift %s 2> %t.warnings.txt
// RUN: %FileCheck -check-prefix=CHECK-WARNINGS %s < %t.warnings.txt

import StdlibUnittest

import Foundation

var tests = TestSuite("NSError")

tests.test("user info") {
  let error = NSError(domain: "MyDomain", code: 1, userInfo: [
      // CHECK-WARNINGS: warning: 'localizedDescriptionKey' is deprecated: renamed to 'NSLocalizedDescriptionKey'
      // CHECK-WARNINGS: note: use 'NSLocalizedDescriptionKey' instead
        AnyHashable(ErrorUserInfoKey.localizedDescriptionKey.rawValue): "description",
        AnyHashable(NSLocalizedFailureReasonErrorKey): "reason"
      ])
  expectEqual("description", error.userInfo[NSLocalizedDescriptionKey]! as! String)

  expectEqual("reason", error.userInfo[ErrorUserInfoKey.localizedFailureReasonErrorKey.rawValue]! as! String)

  // TODO: Without the 'as NSObject' conversion, this produces nil.
  // We may need to forward _CustomAnyHashable through swift_newtypes.
  expectEqual("reason", error.userInfo[ErrorUserInfoKey.localizedFailureReasonErrorKey as NSObject]! as! String)
}

runAllTests()
