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
// RUN: %target-build-swift %s -I%t -L%t -o %t/TestDateComponents
// RUN: %target-run %t/TestDateComponents
//
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import FoundationTestsShared

#if FOUNDATION_XCTEST
import XCTest
class TestDateComponentsSuper : XCTestCase { }
#else
import StdlibUnittest
class TestDateComponentsSuper { }
#endif

class TestDateComponents : TestDateComponentsSuper {
    func test_EncodingRoundTrip_JSON() {
        let components: Set<Calendar.Component> = [
            .era, .year, .month, .day, .hour, .minute, .second, .nanosecond,
            .weekday, .weekdayOrdinal, .quarter, .weekOfMonth, .weekOfYear,
            .yearForWeekOfYear, .timeZone, .calendar
        ]

        let calendar = Calendar(identifier: .gregorian)
        let dateComponents = calendar.dateComponents(components, from: Date())
        expectRoundTripEqualityThroughJSON(for: dateComponents)
    }

    func test_EncodingRoundTrip_Plist() {
        let components: Set<Calendar.Component> = [
            .era, .year, .month, .day, .hour, .minute, .second, .nanosecond,
            .weekday, .weekdayOrdinal, .quarter, .weekOfMonth, .weekOfYear,
            .yearForWeekOfYear, .timeZone, .calendar
        ]

        let calendar = Calendar(identifier: .gregorian)
        let dateComponents = calendar.dateComponents(components, from: Date())
        expectRoundTripEqualityThroughPlist(for: dateComponents)
    }
}

#if !FOUNDATION_XCTEST
var DateComponentsTests = TestSuite("TestDateComponents")
DateComponentsTests.test("test_EncodingRoundTrip_JSON") { TestDateComponents().test_EncodingRoundTrip_JSON() }
DateComponentsTests.test("test_EncodingRoundTrip_Plist") { TestDateComponents().test_EncodingRoundTrip_Plist() }
runAllTests()
#endif
