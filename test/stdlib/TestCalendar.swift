// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %empty-directory(%t)
//
// RUN: %target-clang %S/Inputs/FoundationBridge/FoundationBridge.m -c -o %t/FoundationBridgeObjC.o -g
// RUN: %target-build-swift %s -I %S/Inputs/FoundationBridge/ -Xlinker %t/FoundationBridgeObjC.o -o %t/TestCalendar

// RUN: %target-run %t/TestCalendar > %t.txt
// REQUIRES: executable_test
// REQUIRES: objc_interop

// FIXME: rdar://problem/31207060
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=watchos

import Foundation
import FoundationBridgeObjC

#if FOUNDATION_XCTEST
    import XCTest
    class TestCalendarSuper : XCTestCase { }
#else
    import StdlibUnittest
    class TestCalendarSuper { }
#endif

class TestCalendar : TestCalendarSuper {
    
    func test_copyOnWrite() {
        var c = Calendar(identifier: .gregorian)
        let c2 = c
        expectEqual(c, c2)
        
        // Change the weekday and check result
        let firstWeekday = c.firstWeekday
        let newFirstWeekday = firstWeekday < 7 ? firstWeekday + 1 : firstWeekday - 1
        
        c.firstWeekday = newFirstWeekday
        expectEqual(newFirstWeekday, c.firstWeekday)
        expectEqual(c2.firstWeekday, firstWeekday)
        
        expectNotEqual(c, c2)
        
        // Change the time zone and check result
        let c3 = c
        expectEqual(c, c3)
        
        let tz = c.timeZone
        // Use two different identifiers so we don't fail if the current time zone happens to be the one returned
        let aTimeZoneId = TimeZone.knownTimeZoneIdentifiers[1]
        let anotherTimeZoneId = TimeZone.knownTimeZoneIdentifiers[0]
        
        let newTz = tz.identifier == aTimeZoneId ? TimeZone(identifier: anotherTimeZoneId)! : TimeZone(identifier: aTimeZoneId)!
        
        c.timeZone = newTz
        expectNotEqual(c, c3)
        
    }
    
    func test_bridgingAutoupdating() {
        let tester = CalendarBridgingTester()
        
        do {
            let c = Calendar.autoupdatingCurrent
            let result = tester.verifyAutoupdating(c)
            expectTrue(result)
        }
        
        // Round trip an autoupdating calendar
        do {
            let c = tester.autoupdatingCurrentCalendar()
            let result = tester.verifyAutoupdating(c)
            expectTrue(result)
        }
    }
    
    func test_equality() {
        let autoupdating = Calendar.autoupdatingCurrent
        let autoupdating2 = Calendar.autoupdatingCurrent

        expectEqual(autoupdating, autoupdating2)
        
        let current = Calendar.current
        
        expectNotEqual(autoupdating, current)
        
        // Make a copy of current
        var current2 = current
        expectEqual(current, current2)
        
        // Mutate something (making sure we don't use the current time zone)
        if current2.timeZone.identifier == "America/Los_Angeles" {
            current2.timeZone = TimeZone(identifier: "America/New_York")!
        } else {
            current2.timeZone = TimeZone(identifier: "America/Los_Angeles")!
        }
        expectNotEqual(current, current2)
        
        // Mutate something else
        current2 = current
        expectEqual(current, current2)
        
        current2.locale = Locale(identifier: "MyMadeUpLocale")
        expectNotEqual(current, current2)
    }
    
    func test_properties() {
        // Mainly we want to just make sure these go through to the NSCalendar implementation at this point.
        if #available(iOS 8.0, OSX 10.7, *) {
            var c = Calendar(identifier: .gregorian)
            // Use english localization
            c.locale = Locale(identifier: "en_US")
            c.timeZone = TimeZone(identifier: "America/Los_Angeles")!
            
            expectEqual("AM", c.amSymbol)
            expectEqual("PM", c.pmSymbol)
            expectEqual(["1st quarter", "2nd quarter", "3rd quarter", "4th quarter"], c.quarterSymbols)
            expectEqual(["1st quarter", "2nd quarter", "3rd quarter", "4th quarter"], c.standaloneQuarterSymbols)
            expectEqual(["BC", "AD"], c.eraSymbols)
            expectEqual(["Before Christ", "Anno Domini"], c.longEraSymbols)
            expectEqual(["J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"], c.veryShortMonthSymbols)
            expectEqual(["J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"], c.veryShortStandaloneMonthSymbols)
            expectEqual(["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"], c.shortMonthSymbols)
            expectEqual(["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"], c.shortStandaloneMonthSymbols)
            expectEqual(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], c.monthSymbols)
            expectEqual(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], c.standaloneMonthSymbols)
            expectEqual(["Q1", "Q2", "Q3", "Q4"], c.shortQuarterSymbols)
            expectEqual(["Q1", "Q2", "Q3", "Q4"], c.shortStandaloneQuarterSymbols)
            expectEqual(["S", "M", "T", "W", "T", "F", "S"], c.veryShortStandaloneWeekdaySymbols)
            expectEqual(["S", "M", "T", "W", "T", "F", "S"], c.veryShortWeekdaySymbols)
            expectEqual(["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"], c.shortStandaloneWeekdaySymbols)
            expectEqual(["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"], c.shortWeekdaySymbols)
            expectEqual(["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"], c.standaloneWeekdaySymbols)
            expectEqual(["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"], c.weekdaySymbols)
            
            // The idea behind these tests is not to test calendrical math, but to simply verify that we are getting some kind of result from calling through to the underlying Foundation and ICU logic. If we move that logic into this struct in the future, then we will need to expand the test cases.
            
            // This is a very special Date in my life: the exact moment when I wrote these test cases and therefore knew all of the answers.
            let d = Date(timeIntervalSince1970: 1468705593.2533731)
            let earlierD = c.date(byAdding: DateComponents(day: -10), to: d)!
                
            expectEqual(1..<29, c.minimumRange(of: .day))
            expectEqual(1..<54, c.maximumRange(of: .weekOfYear))
            expectEqual(0..<60, c.range(of: .second, in: .minute, for: d))
            
            var d1 = Date()
            var ti : TimeInterval = 0
            
            expectTrue(c.dateInterval(of: .day, start: &d1, interval: &ti, for: d))
            expectEqual(Date(timeIntervalSince1970: 1468652400.0), d1)
            expectEqual(86400, ti)
            
            if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
                let dateInterval = c.dateInterval(of: .day, for: d)
                expectEqual(DateInterval(start: d1, duration: ti), dateInterval)
            }
            
            expectEqual(15, c.ordinality(of: .hour, in: .day, for: d))
            
            expectEqual(Date(timeIntervalSince1970: 1468791993.2533731), c.date(byAdding: .day, value: 1, to: d))
            expectEqual(Date(timeIntervalSince1970: 1468791993.2533731), c.date(byAdding: DateComponents(day: 1),  to: d))
            
            expectEqual(Date(timeIntervalSince1970: 946627200.0), c.date(from: DateComponents(year: 1999, month: 12, day: 31)))
            
            let comps = c.dateComponents([.year, .month, .day], from: Date(timeIntervalSince1970: 946627200.0))
            expectEqual(1999, comps.year)
            expectEqual(12, comps.month)
            expectEqual(31, comps.day)
            
            expectEqual(10, c.dateComponents([.day], from: d, to: c.date(byAdding: DateComponents(day: 10), to: d)!).day)
            
            expectEqual(30, c.dateComponents([.day], from: DateComponents(year: 1999, month: 12, day: 1), to: DateComponents(year: 1999, month: 12, day: 31)).day)
            
            expectEqual(2016, c.component(.year, from: d))
            
            expectEqual(Date(timeIntervalSince1970: 1468652400.0), c.startOfDay(for: d))
            
            if #available(iOS 8, macOS 10.10, *) {
              // Mac OS X 10.9 and iOS 7 had a bug in NSCalendar for hour, minute, and second granularities.
              expectEqual(.orderedSame, c.compare(d, to: d + 10, toGranularity: .minute))
            }
            
            expectFalse(c.isDate(d, equalTo: d + 10, toGranularity: .second))
            expectTrue(c.isDate(d, equalTo: d + 10, toGranularity: .day))
            
            expectFalse(c.isDate(earlierD, inSameDayAs: d))
            expectTrue(c.isDate(d, inSameDayAs: d))
            
            expectFalse(c.isDateInToday(earlierD))
            expectFalse(c.isDateInYesterday(earlierD))
            expectFalse(c.isDateInTomorrow(earlierD))
            
            expectTrue(c.isDateInWeekend(d)) // 😢
            
            expectTrue(c.dateIntervalOfWeekend(containing: d, start: &d1, interval: &ti))
            
            if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
                let thisWeekend = DateInterval(start: Date(timeIntervalSince1970: 1468652400.0), duration: 172800.0)
                
                expectEqual(thisWeekend, DateInterval(start: d1, duration: ti))
                expectEqual(thisWeekend, c.dateIntervalOfWeekend(containing: d))
            }
            

            expectTrue(c.nextWeekend(startingAfter: d, start: &d1, interval: &ti))
            
            if #available(iOS 10.10, OSX 10.12, tvOS 10.0, watchOS 3.0, *) {
                let nextWeekend = DateInterval(start: Date(timeIntervalSince1970: 1469257200.0), duration: 172800.0)
            
                expectEqual(nextWeekend, DateInterval(start: d1, duration: ti))
                expectEqual(nextWeekend, c.nextWeekend(startingAfter: d))
            }
            
            // Enumeration
            
            var count = 0
            var exactCount = 0
            
            // Find the days numbered '31' after 'd', allowing the algorithm to move to the next day if required
            c.enumerateDates(startingAfter: d, matching: DateComponents(day: 31), matchingPolicy: .nextTime) { result, exact, stop in
                // Just stop some arbitrary time in the future
                if result! > d + 86400*365 { stop = true }
                count += 1
                if exact { exactCount += 1 }
            }
            
            /*
             Optional(2016-07-31 07:00:00 +0000)
             Optional(2016-08-31 07:00:00 +0000)
             Optional(2016-10-01 07:00:00 +0000)
             Optional(2016-10-31 07:00:00 +0000)
             Optional(2016-12-01 08:00:00 +0000)
             Optional(2016-12-31 08:00:00 +0000)
             Optional(2017-01-31 08:00:00 +0000)
             Optional(2017-03-01 08:00:00 +0000)
             Optional(2017-03-31 07:00:00 +0000)
             Optional(2017-05-01 07:00:00 +0000)
             Optional(2017-05-31 07:00:00 +0000)
             Optional(2017-07-01 07:00:00 +0000)
             Optional(2017-07-31 07:00:00 +0000)
             */

            expectEqual(count, 13)
            expectEqual(exactCount, 8)
            
            
            expectEqual(Date(timeIntervalSince1970: 1469948400.0), c.nextDate(after: d, matching: DateComponents(day: 31), matchingPolicy: .nextTime))
            
            
            expectEqual(Date(timeIntervalSince1970: 1468742400.0),  c.date(bySetting: .hour, value: 1, of: d))
            
            expectEqual(Date(timeIntervalSince1970: 1468656123.0), c.date(bySettingHour: 1, minute: 2, second: 3, of: d, matchingPolicy: .nextTime))
            
            expectTrue(c.date(d, matchesComponents: DateComponents(month: 7)))
            expectFalse(c.date(d, matchesComponents: DateComponents(month: 7, day: 31)))
        }
    }

    func test_AnyHashableContainingCalendar() {
        let values: [Calendar] = [
            Calendar(identifier: .gregorian),
            Calendar(identifier: .japanese),
            Calendar(identifier: .japanese)
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Calendar.self, type(of: anyHashables[0].base))
        expectEqual(Calendar.self, type(of: anyHashables[1].base))
        expectEqual(Calendar.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSCalendar() {
        if #available(iOS 8.0, *) {
            let values: [NSCalendar] = [
                NSCalendar(identifier: .gregorian)!,
                NSCalendar(identifier: .japanese)!,
                NSCalendar(identifier: .japanese)!,
            ]
            let anyHashables = values.map(AnyHashable.init)
            expectEqual(Calendar.self, type(of: anyHashables[0].base))
            expectEqual(Calendar.self, type(of: anyHashables[1].base))
            expectEqual(Calendar.self, type(of: anyHashables[2].base))
            expectNotEqual(anyHashables[0], anyHashables[1])
            expectEqual(anyHashables[1], anyHashables[2])
        }
    }
}

#if !FOUNDATION_XCTEST
var CalendarTests = TestSuite("TestCalendar")
CalendarTests.test("test_copyOnWrite") { TestCalendar().test_copyOnWrite() }
CalendarTests.test("test_bridgingAutoupdating") { TestCalendar().test_bridgingAutoupdating() }
CalendarTests.test("test_equality") { TestCalendar().test_equality() }
CalendarTests.test("test_properties") { TestCalendar().test_properties() }
CalendarTests.test("test_AnyHashableContainingCalendar") { TestCalendar().test_AnyHashableContainingCalendar() }
CalendarTests.test("test_AnyHashableCreatedFromNSCalendar") { TestCalendar().test_AnyHashableCreatedFromNSCalendar() }
runAllTests()
#endif
