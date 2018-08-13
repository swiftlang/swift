
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 4 %s -o %t/TestData_Swift4
// RUN: %target-codesign %t/TestData_Swift4
// RUN: %target-run %t/TestData_Swift4
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var DataTests = TestSuite("TestDataSwift4")

DataTests.test("functional map init usage") {
	let res1 = [[UInt8(0), UInt8(1), UInt8(2)]].map(Data.init) // previously this could be done without being ambiguous (however in swift 4.2 an overload was added that makes it ambiguous as a function ref)
	// the following two strategies are preferred to the previous version
	let res2 = [[UInt8(0), UInt8(1), UInt8(2)]].map(Data.init(_:))
	let res3 = [[UInt8(0), UInt8(1), UInt8(2)]].map { Data($0) }

	expectEqual(res1.count, 1)
	expectEqual(res2.count, 1)
	expectEqual(res3.count, 1)

	expectEqual(res1[0], res2[0])
	expectEqual(res2[0], res3[0])
}

runAllTests()
