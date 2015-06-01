// RUN: %target-build-swift -module-name main %s

// REQUIRES: objc_interop

// watchOS 2.0 does not have a public XCTest module.
// XFAIL: OS=watchos

import XCTest

func smokeTest() {
  var subject: AnyObject? = nil
  XCTAssertNil(subject)
}

