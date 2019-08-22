// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

#if FOUNDATION_XCTEST
import XCTest
class TestDateIntervalSuper : XCTestCase { }
#else
import StdlibUnittest
class TestDateIntervalSuper { }
#endif

class TestDateInterval : TestDateIntervalSuper {
    func dateWithString(_ str: String) -> Date {
        let formatter = DateFormatter()
        formatter.calendar = Calendar(identifier: .gregorian)
        formatter.locale = Locale(identifier: "en_US")
        formatter.dateFormat = "yyyy-MM-dd HH:mm:ss Z"
        return formatter.date(from: str)! as Date
    }

    func test_compareDateIntervals() {
        if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
            let start = dateWithString("2010-05-17 14:49:47 -0700")
            let duration: TimeInterval = 10000000.0
            let testInterval1 = DateInterval(start: start, duration: duration)
            let testInterval2 = DateInterval(start: start, duration: duration)
            expectEqual(testInterval1, testInterval2)
            expectEqual(testInterval2, testInterval1)
            expectEqual(testInterval1.compare(testInterval2), ComparisonResult.orderedSame)
            
            let testInterval3 = DateInterval(start: start, duration: 10000000000.0)
            expectTrue(testInterval1 < testInterval3)
            expectTrue(testInterval3 > testInterval1)
            
            let earlierStart = dateWithString("2009-05-17 14:49:47 -0700")
            let testInterval4 = DateInterval(start: earlierStart, duration: duration)
            
            expectTrue(testInterval4 < testInterval1)
            expectTrue(testInterval1 > testInterval4)
        }
    }

    func test_isEqualToDateInterval() {
        if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
            let start = dateWithString("2010-05-17 14:49:47 -0700")
            let duration = 10000000.0
            let testInterval1 = DateInterval(start: start, duration: duration)
            let testInterval2 = DateInterval(start: start, duration: duration)
            
            expectEqual(testInterval1, testInterval2)
            
            let testInterval3 = DateInterval(start: start, duration: 100.0)
            expectNotEqual(testInterval1, testInterval3)
        }
    }

    func test_hashing() {
        guard #available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *) else { return }

        let start1a = dateWithString("2019-04-04 17:09:23 -0700")
        let start1b = dateWithString("2019-04-04 17:09:23 -0700")
        let start2a = Date(timeIntervalSinceReferenceDate: start1a.timeIntervalSinceReferenceDate.nextUp)
        let start2b = Date(timeIntervalSinceReferenceDate: start1a.timeIntervalSinceReferenceDate.nextUp)
        let duration1 = 1800.0
        let duration2 = duration1.nextUp
        let intervals: [[DateInterval]] = [
            [
                DateInterval(start: start1a, duration: duration1),
                DateInterval(start: start1b, duration: duration1),
            ],
            [
                DateInterval(start: start1a, duration: duration2),
                DateInterval(start: start1b, duration: duration2),
            ],
            [
                DateInterval(start: start2a, duration: duration1),
                DateInterval(start: start2b, duration: duration1),
            ],
            [
                DateInterval(start: start2a, duration: duration2),
                DateInterval(start: start2b, duration: duration2),
            ],
        ]
        checkHashableGroups(intervals)
    }

    func test_checkIntersection() {
        if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
            let start1 = dateWithString("2010-05-17 14:49:47 -0700")
            let end1 = dateWithString("2010-08-17 14:49:47 -0700")
            
            let testInterval1 = DateInterval(start: start1, end: end1)
            
            let start2 = dateWithString("2010-02-17 14:49:47 -0700")
            let end2 = dateWithString("2010-07-17 14:49:47 -0700")
            
            let testInterval2 = DateInterval(start: start2, end: end2)
            
            expectTrue(testInterval1.intersects(testInterval2))
            
            let start3 = dateWithString("2010-10-17 14:49:47 -0700")
            let end3 = dateWithString("2010-11-17 14:49:47 -0700")
            
            let testInterval3 = DateInterval(start: start3, end: end3)
            
            expectFalse(testInterval1.intersects(testInterval3))
        }
    }

    func test_validIntersections() {
        if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
            let start1 = dateWithString("2010-05-17 14:49:47 -0700")
            let end1 = dateWithString("2010-08-17 14:49:47 -0700")
            
            let testInterval1 = DateInterval(start: start1, end: end1)
            
            let start2 = dateWithString("2010-02-17 14:49:47 -0700")
            let end2 = dateWithString("2010-07-17 14:49:47 -0700")
            
            let testInterval2 = DateInterval(start: start2, end: end2)
            
            let start3 = dateWithString("2010-05-17 14:49:47 -0700")
            let end3 = dateWithString("2010-07-17 14:49:47 -0700")
            
            let testInterval3 = DateInterval(start: start3, end: end3)
            
            let intersection1 = testInterval2.intersection(with: testInterval1)
            expectNotNil(intersection1)
            expectEqual(testInterval3, intersection1)
            
            let intersection2 = testInterval1.intersection(with: testInterval2)
            expectNotNil(intersection2)
            expectEqual(intersection1, intersection2)
        }
    }

    func test_containsDate() {
        if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
            let start = dateWithString("2010-05-17 14:49:47 -0700")
            let duration = 10000000.0
            
            let testInterval = DateInterval(start: start, duration: duration)
            let containedDate = dateWithString("2010-05-17 20:49:47 -0700")
            
            expectTrue(testInterval.contains(containedDate))
            
            let earlierStart = dateWithString("2009-05-17 14:49:47 -0700")
            expectFalse(testInterval.contains(earlierStart))
        }
    }

    func test_AnyHashableContainingDateInterval() {
        if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
            let start = dateWithString("2010-05-17 14:49:47 -0700")
            let duration = 10000000.0
            let values: [DateInterval] = [
                DateInterval(start: start, duration: duration),
                DateInterval(start: start, duration: duration / 2),
                DateInterval(start: start, duration: duration / 2),
            ]
            let anyHashables = values.map(AnyHashable.init)
            expectEqual(DateInterval.self, type(of: anyHashables[0].base))
            expectEqual(DateInterval.self, type(of: anyHashables[1].base))
            expectEqual(DateInterval.self, type(of: anyHashables[2].base))
            expectNotEqual(anyHashables[0], anyHashables[1])
            expectEqual(anyHashables[1], anyHashables[2])
        }
    }

    func test_AnyHashableCreatedFromNSDateInterval() {
        if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
            let start = dateWithString("2010-05-17 14:49:47 -0700")
            let duration = 10000000.0
            let values: [NSDateInterval] = [
                NSDateInterval(start: start, duration: duration),
                NSDateInterval(start: start, duration: duration / 2),
                NSDateInterval(start: start, duration: duration / 2),
            ]
            let anyHashables = values.map(AnyHashable.init)
            expectEqual(DateInterval.self, type(of: anyHashables[0].base))
            expectEqual(DateInterval.self, type(of: anyHashables[1].base))
            expectEqual(DateInterval.self, type(of: anyHashables[2].base))
            expectNotEqual(anyHashables[0], anyHashables[1])
            expectEqual(anyHashables[1], anyHashables[2])
        }
    }
}

#if !FOUNDATION_XCTEST
var DateIntervalTests = TestSuite("TestDateInterval")
DateIntervalTests.test("test_compareDateIntervals") { TestDateInterval().test_compareDateIntervals() }
DateIntervalTests.test("test_isEqualToDateInterval") { TestDateInterval().test_isEqualToDateInterval() }
DateIntervalTests.test("test_hashing") { TestDateInterval().test_hashing() }
DateIntervalTests.test("test_checkIntersection") { TestDateInterval().test_checkIntersection() }
DateIntervalTests.test("test_validIntersections") { TestDateInterval().test_validIntersections() }
DateIntervalTests.test("test_AnyHashableContainingDateInterval") { TestDateInterval().test_AnyHashableContainingDateInterval() }
DateIntervalTests.test("test_AnyHashableCreatedFromNSDateInterval") { TestDateInterval().test_AnyHashableCreatedFromNSDateInterval() }
runAllTests()
#endif
