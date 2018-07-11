// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

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
        ErrorUserInfoKey.localizedDescriptionKey.rawValue: "description",
        NSLocalizedFailureReasonErrorKey: "reason"
      ])
  expectEqual("description", error.userInfo[NSLocalizedDescriptionKey]! as! String)

  expectEqual("reason", error.userInfo[ErrorUserInfoKey.localizedFailureReasonErrorKey.rawValue]! as! String)

  // TODO: Without the 'as NSObject' conversion, this produces nil.
  // We may need to forward _CustomAnyHashable through swift_newtypes.
  expectEqual("reason", error.userInfo[ErrorUserInfoKey.localizedFailureReasonErrorKey.rawValue]! as! String)
}

tests.test("convenience") {
    let error1 = CocoaError.error(.fileNoSuchFile)
    expectNotNil((error1 as NSError).localizedDescription)
    expectEqual(CocoaError.Code.fileNoSuchFile.rawValue, (error1 as NSError).code)
    
    let url = URL(string: "file:///tmp/bar/foo")
    let error2 = CocoaError.error(.fileNoSuchFile, url: url)
    expectEqual(url, (error2 as! CocoaError).url)
    expectNotNil((error2 as NSError).localizedDescription)
    expectEqual(url, (error2 as NSError).userInfo[NSURLErrorKey] as? URL)
    
    let error3 = CocoaError.error(.fileNoSuchFile, userInfo: ["foo" : "bar"], url: url)
    expectEqual(url, (error3 as! CocoaError).url)
    expectNotNil((error3 as NSError).localizedDescription)
    expectEqual(url, (error3 as NSError).userInfo[NSURLErrorKey] as? URL)
    expectEqual("bar", (error3 as NSError).userInfo["foo"] as? String)
}

tests.test("Hashable") {
  checkHashable([CocoaError.Code.fileNoSuchFile, .fileReadUnknown, .keyValueValidation], equalityOracle: { $0 == $1 })
  checkHashable([URLError.Code.unknown, .cancelled, .badURL], equalityOracle: { $0 == $1 })
}

runAllTests()
