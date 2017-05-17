
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: pushd %t
// RUN: %target-build-swift -emit-module -emit-library %S/Inputs/FoundationTestsShared.swift -module-name FoundationTestsShared -module-link-name FoundationTestsShared
// RUN: popd %t
//
// RUN: %target-build-swift %s -I%t -L%t -o %t/TestNSRange
// RUN: %target-run %t/TestNSRange
//
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import FoundationTestsShared

#if FOUNDATION_XCTEST
import XCTest
class TestNSRangeSuper : XCTestCase { }
#else
import StdlibUnittest
class TestNSRangeSuper { }
#endif

class TestNSRange : TestNSRangeSuper {
    func test_EncodingRoundTrip_JSON() {
        let values: [NSRange] = [
            NSRange(),
            NSRange(location: 0, length: Int.max),
            NSRange(location: NSNotFound, length: 0)
        ]

        for range in values {
            expectRoundTripEqualityThroughJSON(for: range)
        }
    }

    func test_EncodingRoundTrip_Plist() {
        let values: [NSRange] = [
            NSRange(),
            NSRange(location: 0, length: Int.max),
            NSRange(location: NSNotFound, length: 0)
        ]

        for range in values {
            expectRoundTripEqualityThroughPlist(for: range)
        }
    }
}

#if !FOUNDATION_XCTEST
var NSRangeTests = TestSuite("TestNSRange")
NSRangeTests.test("test_EncodingRoundTrip_JSON") { TestNSRange().test_EncodingRoundTrip_JSON() }
NSRangeTests.test("test_EncodingRoundTrip_Plist") { TestNSRange().test_EncodingRoundTrip_Plist() }
runAllTests()
#endif
