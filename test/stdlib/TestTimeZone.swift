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
// RUN: %target-clang %S/Inputs/FoundationBridge/FoundationBridge.m -c -o %t/FoundationBridgeObjC.o -g
// RUN: %target-build-swift %s -I %S/Inputs/FoundationBridge/ -Xlinker %t/FoundationBridgeObjC.o -o %t/TestTimeZone

// RUN: %target-run %t/TestTimeZone > %t.txt
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import FoundationBridgeObjC

#if FOUNDATION_XCTEST
    import XCTest
    class TestTimeZoneSuper : XCTestCase { }
#else
    import StdlibUnittest
    class TestTimeZoneSuper { }
#endif

class TestTimeZone : TestTimeZoneSuper {
    
    func test_timeZoneBasics() {
        let tz = TimeZone(identifier: "America/Los_Angeles")!
        
        expectTrue(!tz.identifier.isEmpty)
    }
    
    func test_bridgingAutoupdating() {
        let tester = TimeZoneBridgingTester()
        
        do {
            let tz = TimeZone.autoupdatingCurrent
            let result = tester.verifyAutoupdating(tz)
            expectTrue(result)
        }
        
        // Round trip an autoupdating calendar
        do {
            let tz = tester.autoupdatingCurrentTimeZone()
            let result = tester.verifyAutoupdating(tz)
            expectTrue(result)
        }
    }
    
    func test_equality() {
        let autoupdating = TimeZone.autoupdatingCurrent
        let autoupdating2 = TimeZone.autoupdatingCurrent

        expectEqual(autoupdating, autoupdating2)
        
        let current = TimeZone.current
        
        expectNotEqual(autoupdating, current)
    }

    func test_AnyHashableContainingTimeZone() {
        let values: [TimeZone] = [
            TimeZone(identifier: "America/Los_Angeles")!,
            TimeZone(identifier: "Europe/Kiev")!,
            TimeZone(identifier: "Europe/Kiev")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(TimeZone.self, type(of: anyHashables[0].base))
        expectEqual(TimeZone.self, type(of: anyHashables[1].base))
        expectEqual(TimeZone.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSTimeZone() {
        let values: [NSTimeZone] = [
            NSTimeZone(name: "America/Los_Angeles")!,
            NSTimeZone(name: "Europe/Kiev")!,
            NSTimeZone(name: "Europe/Kiev")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(TimeZone.self, type(of: anyHashables[0].base))
        expectEqual(TimeZone.self, type(of: anyHashables[1].base))
        expectEqual(TimeZone.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }
}

#if !FOUNDATION_XCTEST
var TimeZoneTests = TestSuite("TestTimeZone")
TimeZoneTests.test("test_timeZoneBasics") { TestTimeZone().test_timeZoneBasics() }
TimeZoneTests.test("test_bridgingAutoupdating") { TestTimeZone().test_bridgingAutoupdating() }
TimeZoneTests.test("test_equality") { TestTimeZone().test_equality() }
TimeZoneTests.test("test_AnyHashableContainingTimeZone") { TestTimeZone().test_AnyHashableContainingTimeZone() }
TimeZoneTests.test("test_AnyHashableCreatedFromNSTimeZone") { TestTimeZone().test_AnyHashableCreatedFromNSTimeZone() }
runAllTests()
#endif
