import XCTest

import swiftInspectTests

var tests = [XCTestCaseEntry]()
tests += swiftInspectTests.allTests()
XCTMain(tests)
