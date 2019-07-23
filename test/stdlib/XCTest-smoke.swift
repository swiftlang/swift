// RUN: %empty-directory(%t)
// RUN: not test -e %platform-sdk-overlay-dir/XCTest.swiftmodule || %target-build-swift %s -o %t/main

// REQUIRES: objc_interop

import XCTest

// Check that we can actually refer to functions from the XCTest overlay.
let optionalInt: Int? = nil
XCTAssertNil(optionalInt)

// Check that the underlying framework was imported.
_ = XCTestCase.self
