// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import StdlibUnittest

import Foundation
import SafariServices

var SafariServicesTests = TestSuite("SafariServices")
if #available(OSX 10.12, *) {
  SafariServicesTests.test("API") {
	_ = SFSafariServicesAvailable()
	_ = SFSafariServicesAvailable(.version10_0)
    _ = SFSafariServicesAvailable(.version10_1)
  }
}
runAllTests()
