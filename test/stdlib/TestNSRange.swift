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
class TestNSRangeSuper : XCTestCase { }
#else
import StdlibUnittest
class TestNSRangeSuper { }
#endif

class TestNSRange : TestNSRangeSuper {
    func testEquality() {
        let r1 = NSRange(location: 1, length: 10)
        let r2 = NSRange(location: 1, length: 11)
        let r3 = NSRange(location: 2, length: 10)
        let r4 = NSRange(location: 1, length: 10)
        let r5 = NSRange(location: NSNotFound, length: 0)
        let r6 = NSRange(location: NSNotFound, length: 2)

        expectNotEqual(r1, r2)
        expectNotEqual(r1, r3)
        expectEqual(r1, r4)
        expectNotEqual(r1, r5)
        expectNotEqual(r5, r6)
    }

    func testDescription() {
        let r1 = NSRange(location: 0, length: 22)
        let r2 = NSRange(location: 10, length: 22)
        let r3 = NSRange(location: NSNotFound, length: 0)
        let r4 = NSRange(location: NSNotFound, length: 22)
        expectEqual("{0, 22}", r1.description)
        expectEqual("{10, 22}", r2.description)
        expectEqual("{\(NSNotFound), 0}", r3.description)
        expectEqual("{\(NSNotFound), 22}", r4.description)

        expectEqual("{0, 22}", r1.debugDescription)
        expectEqual("{10, 22}", r2.debugDescription)
        expectEqual("{NSNotFound, 0}", r3.debugDescription)
        expectEqual("{NSNotFound, 22}", r4.debugDescription)
    }

    func testCreationFromString() {
        let r1 = NSRange("")
        expectNil(r1)
        let r2 = NSRange("1")
        expectNil(r2)
        let r3 = NSRange("1 2")
        expectEqual(NSRange(location: 1, length: 2), r3)
        let r4 = NSRange("{1 8")
        expectEqual(NSRange(location: 1, length: 8), r4)
        let r5 = NSRange("1.8")
        expectNil(r5)
        let r6 = NSRange("1-9")
        expectEqual(NSRange(location: 1, length: 9), r6)
        let r7 = NSRange("{1,9}")
        expectEqual(NSRange(location: 1, length: 9), r7)
        let r8 = NSRange("{1,9}asdfasdf")
        expectEqual(NSRange(location: 1, length: 9), r8)
        let r9 = NSRange("{1,9}{2,7}")
        expectEqual(NSRange(location: 1, length: 9), r9)
        let r10 = NSRange("{１,９}")        
        expectEqual(NSRange(location: 1, length: 9), r10)
        let r11 = NSRange("{1.0,9}")
        expectEqual(NSRange(location: 1, length: 9), r11)
        let r12 = NSRange("{1,9.0}")
        expectEqual(NSRange(location: 1, length: 9), r12)
        let r13 = NSRange("{1.2,9}")
        expectNil(r13)
        let r14 = NSRange("{1,9.8}")
        expectNil(r14)
    }

    func testHashing() {
        let large = Int.max >> 2
        let samples: [NSRange] = [
            NSRange(location: 1, length: 1),
            NSRange(location: 1, length: 2),
            NSRange(location: 2, length: 1),
            NSRange(location: 2, length: 2),
            NSRange(location: large, length: large),
            NSRange(location: 0, length: large),
            NSRange(location: large, length: 0),
        ]
        checkHashable(samples, equalityOracle: { $0 == $1 })
    }

    func testBounding() {
        let r1 = NSRange(location: 1000, length: 2222)
        expectEqual(r1.location, r1.lowerBound)
        expectEqual(r1.location + r1.length, r1.upperBound)
    }

    func testContains() {
        let r1 = NSRange(location: 1000, length: 2222)
        expectFalse(r1.contains(3))
        expectTrue(r1.contains(1001))
        expectFalse(r1.contains(4000))
    }

    func testUnion() {
        let r1 = NSRange(location: 10, length: 20)
        let r2 = NSRange(location: 30, length: 5)
        let union1 = r1.union(r2)

        expectEqual(Swift.min(r1.lowerBound, r2.lowerBound), union1.lowerBound)
        expectEqual(Swift.max(r1.upperBound, r2.upperBound), union1.upperBound)

        let r3 = NSRange(location: 10, length: 20)
        let r4 = NSRange(location: 11, length: 5)
        let union2 = r3.union(r4)

        expectEqual(Swift.min(r3.lowerBound, r4.lowerBound), union2.lowerBound)
        expectEqual(Swift.max(r3.upperBound, r4.upperBound), union2.upperBound)
        
        let r5 = NSRange(location: 10, length: 20)
        let r6 = NSRange(location: 11, length: 29)
        let union3 = r5.union(r6)
        
        expectEqual(Swift.min(r5.lowerBound, r6.upperBound), union3.lowerBound)
        expectEqual(Swift.max(r5.upperBound, r6.upperBound), union3.upperBound)
    }

    func testIntersection() {
        let r1 = NSRange(location: 1, length: 7)
        let r2 = NSRange(location: 2, length: 20)
        let r3 = NSRange(location: 2, length: 2)
        let r4 = NSRange(location: 10, length: 7)

        let intersection1 = r1.intersection(r2)
        expectEqual(NSRange(location: 2, length: 6), intersection1)
        let intersection2 = r1.intersection(r3)
        expectEqual(NSRange(location: 2, length: 2), intersection2)
        let intersection3 = r1.intersection(r4)
        expectEqual(nil, intersection3)
    }
}

#if !FOUNDATION_XCTEST
var NSRangeTests = TestSuite("TestNSRange")

NSRangeTests.test("testEquality") { TestNSRange().testEquality() }
NSRangeTests.test("testDescription") { TestNSRange().testDescription() }
NSRangeTests.test("testCreationFromString") { TestNSRange().testCreationFromString() }
NSRangeTests.test("testHashing") { TestNSRange().testHashing() }
NSRangeTests.test("testBounding") { TestNSRange().testBounding() }
NSRangeTests.test("testContains") { TestNSRange().testContains() }
NSRangeTests.test("testUnion") { TestNSRange().testUnion() }
NSRangeTests.test("testIntersection") { TestNSRange().testIntersection() }

runAllTests()
#endif
