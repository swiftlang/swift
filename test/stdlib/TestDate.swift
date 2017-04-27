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
import CoreFoundation

#if FOUNDATION_XCTEST
import XCTest
class TestDateSuper : XCTestCase { }
#else
import StdlibUnittest
class TestDateSuper { }
#endif

class TestDate : TestDateSuper {

    func testDateComparison() {
        let d1 = Date()
        let d2 = d1 + 1
        
        expectTrue(d2 > d1)
        expectTrue(d1 < d2)
        
        let d3 = Date(timeIntervalSince1970: 12345)
        let d4 = Date(timeIntervalSince1970: 12345)
        
        expectTrue(d3 == d4)
        expectTrue(d3 <= d4)
        expectTrue(d4 >= d3)
    }
    
    func testDateMutation() {
        let d0 = Date()
        var d1 = Date()
        d1 = d1 + 1
        let d2 = Date(timeIntervalSinceNow: 10)
        
        expectTrue(d2 > d1)
        expectTrue(d1 != d0)
        
        let d3 = d1
        d1 += 10
        expectTrue(d1 > d3)
    }

    func testCast() {
        let d0 = NSDate()
        let d1 = d0 as Date
        expectEqual(d0.timeIntervalSinceReferenceDate, d1.timeIntervalSinceReferenceDate)
    }

    func testDistantPast() {
        let distantPast = Date.distantPast
        let currentDate = Date()
        expectTrue(distantPast < currentDate)
        expectTrue(currentDate > distantPast)
        expectTrue(distantPast.timeIntervalSince(currentDate) < 3600.0*24*365*100) /* ~1 century in seconds */
    }

    func testDistantFuture() {
        let distantFuture = Date.distantFuture
        let currentDate = Date()
        expectTrue(currentDate < distantFuture)
        expectTrue(distantFuture > currentDate)
        expectTrue(distantFuture.timeIntervalSince(currentDate) > 3600.0*24*365*100) /* ~1 century in seconds */
    }

    func dateWithString(_ str: String) -> Date {
        let formatter = DateFormatter()
        // Note: Calendar(identifier:) is OSX 10.9+ and iOS 8.0+ whereas the CF version has always been available
        formatter.calendar = Calendar(identifier: .gregorian)
        formatter.locale = Locale(identifier: "en_US")
        formatter.dateFormat = "yyyy-MM-dd HH:mm:ss Z"
        return formatter.date(from: str)! as Date
    }

    func testEquality() {
        let date = dateWithString("2010-05-17 14:49:47 -0700")
        let sameDate = dateWithString("2010-05-17 14:49:47 -0700")
        expectEqual(date, sameDate)
        expectEqual(sameDate, date)

        let differentDate = dateWithString("2010-05-17 14:49:46 -0700")
        expectNotEqual(date, differentDate)
        expectNotEqual(differentDate, date)

        let sameDateByTimeZone = dateWithString("2010-05-17 13:49:47 -0800")
        expectEqual(date, sameDateByTimeZone)
        expectEqual(sameDateByTimeZone, date)

        let differentDateByTimeZone = dateWithString("2010-05-17 14:49:47 -0800")
        expectNotEqual(date, differentDateByTimeZone)
        expectNotEqual(differentDateByTimeZone, date)
    }

    func testTimeIntervalSinceDate() {
        let referenceDate = dateWithString("1900-01-01 00:00:00 +0000")
        let sameDate = dateWithString("1900-01-01 00:00:00 +0000")
        let laterDate = dateWithString("2010-05-17 14:49:47 -0700")
        let earlierDate = dateWithString("1810-05-17 14:49:47 -0700")

        let laterSeconds = laterDate.timeIntervalSince(referenceDate)
        expectEqual(laterSeconds, 3483121787.0)

        let earlierSeconds = earlierDate.timeIntervalSince(referenceDate)
        expectEqual(earlierSeconds, -2828311813.0)

        let sameSeconds = sameDate.timeIntervalSince(referenceDate)
        expectEqual(sameSeconds, 0.0)
    }
    
    func testDateComponents() {
        // Make sure the optional init stuff works
        let dc = DateComponents()
        
        expectNil(dc.year)
        
        let dc2 = DateComponents(year: 1999)
        
        expectNil(dc2.day)
        expectEqual(1999, dc2.year)
    }

    func test_AnyHashableContainingDate() {
        let values: [Date] = [
            dateWithString("2016-05-17 14:49:47 -0700"),
            dateWithString("2010-05-17 14:49:47 -0700"),
            dateWithString("2010-05-17 14:49:47 -0700"),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Date.self, type(of: anyHashables[0].base))
        expectEqual(Date.self, type(of: anyHashables[1].base))
        expectEqual(Date.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSDate() {
        let values: [NSDate] = [
            NSDate(timeIntervalSince1970: 1000000000),
            NSDate(timeIntervalSince1970: 1000000001),
            NSDate(timeIntervalSince1970: 1000000001),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Date.self, type(of: anyHashables[0].base))
        expectEqual(Date.self, type(of: anyHashables[1].base))
        expectEqual(Date.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableContainingDateComponents() {
        let values: [DateComponents] = [
            DateComponents(year: 2016),
            DateComponents(year: 1995),
            DateComponents(year: 1995),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(DateComponents.self, type(of: anyHashables[0].base))
        expectEqual(DateComponents.self, type(of: anyHashables[1].base))
        expectEqual(DateComponents.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSDateComponents() {
        func makeNSDateComponents(year: Int) -> NSDateComponents {
            let result = NSDateComponents()
            result.year = year
            return result
        }
        let values: [NSDateComponents] = [
            makeNSDateComponents(year: 2016),
            makeNSDateComponents(year: 1995),
            makeNSDateComponents(year: 1995),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(DateComponents.self, type(of: anyHashables[0].base))
        expectEqual(DateComponents.self, type(of: anyHashables[1].base))
        expectEqual(DateComponents.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }
}

#if !FOUNDATION_XCTEST
var DateTests = TestSuite("TestDate")
DateTests.test("testDateComparison") { TestDate().testDateComparison() }
DateTests.test("testDateMutation") { TestDate().testDateMutation() }
DateTests.test("testCast") { TestDate().testCast() }
DateTests.test("testDistantPast") { TestDate().testDistantPast() }
DateTests.test("testDistantFuture") { TestDate().testDistantFuture() }
DateTests.test("testEquality") { TestDate().testEquality() }
DateTests.test("testTimeIntervalSinceDate") { TestDate().testTimeIntervalSinceDate() }
DateTests.test("testDateComponents") { TestDate().testDateComponents() }
DateTests.test("test_AnyHashableContainingDate") { TestDate().test_AnyHashableContainingDate() }
DateTests.test("test_AnyHashableCreatedFromNSDate") { TestDate().test_AnyHashableCreatedFromNSDate() }
DateTests.test("test_AnyHashableContainingDateComponents") { TestDate().test_AnyHashableContainingDateComponents() }
DateTests.test("test_AnyHashableCreatedFromNSDateComponents") { TestDate().test_AnyHashableCreatedFromNSDateComponents() }
runAllTests()
#endif
