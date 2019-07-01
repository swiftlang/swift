// RUN: %empty-directory(%t)
// RUN: not test -e %platform-sdk-overlay-dir/XCTest.swiftmodule || %target-build-swift %s -o %t/main

// REQUIRES: objc_interop

import StdlibUnittest
import XCTest

var xctest = TestSuite("XCTest")

xctest.test("XCTest/XCTAssertEqualWithAccuracy") {
    XCTAssertEqual(0 as Float, 0, accuracy: 1e-4)
    XCTAssertEqual(0 as Double, 0, accuracy: 1e-4)
    XCTAssertEqual(0 as CGFloat, 0, accuracy: 1e-4)
    XCTAssertEqual(0 as Float80, 0, accuracy: 1e-4)
}

xctest.test("XCTest/XCTAssertNotEqualWithAccuracy") {
    XCTAssertNotEqual(0 as Float, 1, accuracy: 1e-4)
    XCTAssertNotEqual(0 as Double, 1, accuracy: 1e-4)
    XCTAssertNotEqual(0 as CGFloat, 1, accuracy: 1e-4)
    XCTAssertNotEqual(0 as Float80, 1, accuracy: 1e-4)
}

runAllTests()