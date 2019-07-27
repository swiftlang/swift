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
class TestIndexPathSuper : XCTestCase { }
#else
import StdlibUnittest
class TestIndexPathSuper { }
#endif

class TestIndexPath: TestIndexPathSuper {
    func testEmpty() {
        let ip = IndexPath()
        expectEqual(ip.count, 0)
    }
    
    func testSingleIndex() {
        let ip = IndexPath(index: 1)
        expectEqual(ip.count, 1)
        expectEqual(ip[0], 1)
        
        let highValueIp = IndexPath(index: .max)
        expectEqual(highValueIp.count, 1)
        expectEqual(highValueIp[0], .max)
        
        let lowValueIp = IndexPath(index: .min)
        expectEqual(lowValueIp.count, 1)
        expectEqual(lowValueIp[0], .min)
    }
    
    func testTwoIndexes() {
        let ip = IndexPath(indexes: [0, 1])
        expectEqual(ip.count, 2)
        expectEqual(ip[0], 0)
        expectEqual(ip[1], 1)
    }
    
    func testManyIndexes() {
        let ip = IndexPath(indexes: [0, 1, 2, 3, 4])
        expectEqual(ip.count, 5)
        expectEqual(ip[0], 0)
        expectEqual(ip[1], 1)
        expectEqual(ip[2], 2)
        expectEqual(ip[3], 3)
        expectEqual(ip[4], 4)
    }
    
    func testCreateFromSequence() {
        let seq = repeatElement(5, count: 3)
        let ip = IndexPath(indexes: seq)
        expectEqual(ip.count, 3)
        expectEqual(ip[0], 5)
        expectEqual(ip[1], 5)
        expectEqual(ip[2], 5)
    }
    
    func testCreateFromLiteral() {
        let ip: IndexPath = [1, 2, 3, 4]
        expectEqual(ip.count, 4)
        expectEqual(ip[0], 1)
        expectEqual(ip[1], 2)
        expectEqual(ip[2], 3)
        expectEqual(ip[3], 4)
    }
    
    func testDropLast() {
        let ip: IndexPath = [1, 2, 3, 4]
        let ip2 = ip.dropLast()
        expectEqual(ip2.count, 3)
        expectEqual(ip2[0], 1)
        expectEqual(ip2[1], 2)
        expectEqual(ip2[2], 3)
    }
    
    func testDropLastFromEmpty() {
        let ip: IndexPath = []
        let ip2 = ip.dropLast()
        expectEqual(ip2.count, 0)
    }
    
    func testDropLastFromSingle() {
        let ip: IndexPath = [1]
        let ip2 = ip.dropLast()
        expectEqual(ip2.count, 0)
    }
    
    func testDropLastFromPair() {
        let ip: IndexPath = [1, 2]
        let ip2 = ip.dropLast()
        expectEqual(ip2.count, 1)
        expectEqual(ip2[0], 1)
    }
    
    func testDropLastFromTriple() {
        let ip: IndexPath = [1, 2, 3]
        let ip2 = ip.dropLast()
        expectEqual(ip2.count, 2)
        expectEqual(ip2[0], 1)
        expectEqual(ip2[1], 2)
    }
    
    func testStartEndIndex() {
        let ip: IndexPath = [1, 2, 3, 4]
        expectEqual(ip.startIndex, 0)
        expectEqual(ip.endIndex, ip.count)
    }
    
    func testIterator() {
        let ip: IndexPath = [1, 2, 3, 4]
        var iter = ip.makeIterator()
        var sum = 0
        while let index = iter.next() {
            sum += index
        }
        expectEqual(sum, 1 + 2 + 3 + 4)
    }
    
    func testIndexing() {
        let ip: IndexPath = [1, 2, 3, 4]
        expectEqual(ip.index(before: 1), 0)
        expectEqual(ip.index(before: 0), -1) // beyond range!
        expectEqual(ip.index(after: 1), 2)
        expectEqual(ip.index(after: 4), 5) // beyond range!
    }
    
    func testCompare() {
        let ip1: IndexPath = [1, 2]
        let ip2: IndexPath = [3, 4]
        let ip3: IndexPath = [5, 1]
        let ip4: IndexPath = [1, 1, 1]
        let ip5: IndexPath = [1, 1, 9]
        
        expectEqual(ip1.compare(ip1), ComparisonResult.orderedSame)
        expectEqual(ip1 < ip1, false)
        expectEqual(ip1 <= ip1, true)
        expectEqual(ip1 == ip1, true)
        expectEqual(ip1 >= ip1, true)
        expectEqual(ip1 > ip1, false)
        
        expectEqual(ip1.compare(ip2), ComparisonResult.orderedAscending)
        expectEqual(ip1 < ip2, true)
        expectEqual(ip1 <= ip2, true)
        expectEqual(ip1 == ip2, false)
        expectEqual(ip1 >= ip2, false)
        expectEqual(ip1 > ip2, false)
        
        expectEqual(ip1.compare(ip3), ComparisonResult.orderedAscending)
        expectEqual(ip1 < ip3, true)
        expectEqual(ip1 <= ip3, true)
        expectEqual(ip1 == ip3, false)
        expectEqual(ip1 >= ip3, false)
        expectEqual(ip1 > ip3, false)
        
        expectEqual(ip1.compare(ip4), ComparisonResult.orderedDescending)
        expectEqual(ip1 < ip4, false)
        expectEqual(ip1 <= ip4, false)
        expectEqual(ip1 == ip4, false)
        expectEqual(ip1 >= ip4, true)
        expectEqual(ip1 > ip4, true)
        
        expectEqual(ip1.compare(ip5), ComparisonResult.orderedDescending)
        expectEqual(ip1 < ip5, false)
        expectEqual(ip1 <= ip5, false)
        expectEqual(ip1 == ip5, false)
        expectEqual(ip1 >= ip5, true)
        expectEqual(ip1 > ip5, true)
        
        expectEqual(ip2.compare(ip1), ComparisonResult.orderedDescending)
        expectEqual(ip2 < ip1, false)
        expectEqual(ip2 <= ip1, false)
        expectEqual(ip2 == ip1, false)
        expectEqual(ip2 >= ip1, true)
        expectEqual(ip2 > ip1, true)
        
        expectEqual(ip2.compare(ip2), ComparisonResult.orderedSame)
        expectEqual(ip2 < ip2, false)
        expectEqual(ip2 <= ip2, true)
        expectEqual(ip2 == ip2, true)
        expectEqual(ip2 >= ip2, true)
        expectEqual(ip2 > ip2, false)
        
        expectEqual(ip2.compare(ip3), ComparisonResult.orderedAscending)
        expectEqual(ip2 < ip3, true)
        expectEqual(ip2 <= ip3, true)
        expectEqual(ip2 == ip3, false)
        expectEqual(ip2 >= ip3, false)
        expectEqual(ip2 > ip3, false)
        
        expectEqual(ip2.compare(ip4), ComparisonResult.orderedDescending)
        expectEqual(ip2.compare(ip5), ComparisonResult.orderedDescending)
        expectEqual(ip3.compare(ip1), ComparisonResult.orderedDescending)
        expectEqual(ip3.compare(ip2), ComparisonResult.orderedDescending)
        expectEqual(ip3.compare(ip3), ComparisonResult.orderedSame)
        expectEqual(ip3.compare(ip4), ComparisonResult.orderedDescending)
        expectEqual(ip3.compare(ip5), ComparisonResult.orderedDescending)
        expectEqual(ip4.compare(ip1), ComparisonResult.orderedAscending)
        expectEqual(ip4.compare(ip2), ComparisonResult.orderedAscending)
        expectEqual(ip4.compare(ip3), ComparisonResult.orderedAscending)
        expectEqual(ip4.compare(ip4), ComparisonResult.orderedSame)
        expectEqual(ip4.compare(ip5), ComparisonResult.orderedAscending)
        expectEqual(ip5.compare(ip1), ComparisonResult.orderedAscending)
        expectEqual(ip5.compare(ip2), ComparisonResult.orderedAscending)
        expectEqual(ip5.compare(ip3), ComparisonResult.orderedAscending)
        expectEqual(ip5.compare(ip4), ComparisonResult.orderedDescending)
        expectEqual(ip5.compare(ip5), ComparisonResult.orderedSame)
        
        let ip6: IndexPath = [1, 1]
        expectEqual(ip6.compare(ip5), ComparisonResult.orderedAscending)
        expectEqual(ip5.compare(ip6), ComparisonResult.orderedDescending)
    }
    
    func testHashing() {
        guard #available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *) else { return }
        let samples: [IndexPath] = [
            [],
            [1],
            [2],
            [Int.max],
            [1, 1],
            [2, 1],
            [1, 2],
            [1, 1, 1],
            [2, 1, 1],
            [1, 2, 1],
            [1, 1, 2],
            [Int.max, Int.max, Int.max],
        ]
        checkHashable(samples, equalityOracle: { $0 == $1 })

        // this should not cause an overflow crash
        _ = IndexPath(indexes: [Int.max >> 8, 2, Int.max >> 36]).hashValue 
    }
    
    func testEquality() {
        let ip1: IndexPath = [1, 1]
        let ip2: IndexPath = [1, 1]
        let ip3: IndexPath = [1, 1, 1]
        let ip4: IndexPath = []
        let ip5: IndexPath = [1]
        
        expectTrue(ip1 == ip2)
        expectFalse(ip1 == ip3)
        expectFalse(ip1 == ip4)
        expectFalse(ip4 == ip1)
        expectFalse(ip5 == ip1)
        expectFalse(ip5 == ip4)
        expectTrue(ip4 == ip4)
        expectTrue(ip5 == ip5)
    }
    
    func testSubscripting() {
        var ip1: IndexPath = [1]
        var ip2: IndexPath = [1, 2]
        var ip3: IndexPath = [1, 2, 3]
        
        expectEqual(ip1[0], 1)
        
        expectEqual(ip2[0], 1)
        expectEqual(ip2[1], 2)
        
        expectEqual(ip3[0], 1)
        expectEqual(ip3[1], 2)
        expectEqual(ip3[2], 3)
        
        ip1[0] = 2
        expectEqual(ip1[0], 2)
        
        ip2[0] = 2
        ip2[1] = 3
        expectEqual(ip2[0], 2)
        expectEqual(ip2[1], 3)
        
        ip3[0] = 2
        ip3[1] = 3
        ip3[2] = 4
        expectEqual(ip3[0], 2)
        expectEqual(ip3[1], 3)
        expectEqual(ip3[2], 4)
        
        let ip4 = ip3[0..<2]
        expectEqual(ip4.count, 2)
        expectEqual(ip4[0], 2)
        expectEqual(ip4[1], 3)
    }
    
    func testAppending() {
        var ip : IndexPath = [1, 2, 3, 4]
        let ip2 = IndexPath(indexes: [5, 6, 7])
        
        ip.append(ip2)
        
        expectEqual(ip.count, 7)
        expectEqual(ip[0], 1)
        expectEqual(ip[6], 7)
        
        let ip3 = ip.appending(IndexPath(indexes: [8, 9]))
        expectEqual(ip3.count, 9)
        expectEqual(ip3[7], 8)
        expectEqual(ip3[8], 9)
        
        let ip4 = ip3.appending([10, 11])
        expectEqual(ip4.count, 11)
        expectEqual(ip4[9], 10)
        expectEqual(ip4[10], 11)
        
        let ip5 = ip.appending(8)
        expectEqual(ip5.count, 8)
        expectEqual(ip5[7], 8)
    }
    
    func testAppendEmpty() {
        var ip: IndexPath = []
        ip.append(1)
        
        expectEqual(ip.count, 1)
        expectEqual(ip[0], 1)
        
        ip.append(2)
        expectEqual(ip.count, 2)
        expectEqual(ip[0], 1)
        expectEqual(ip[1], 2)
        
        ip.append(3)
        expectEqual(ip.count, 3)
        expectEqual(ip[0], 1)
        expectEqual(ip[1], 2)
        expectEqual(ip[2], 3)
        
        ip.append(4)
        expectEqual(ip.count, 4)
        expectEqual(ip[0], 1)
        expectEqual(ip[1], 2)
        expectEqual(ip[2], 3)
        expectEqual(ip[3], 4)
    }
    
    func testAppendEmptyIndexPath() {
        var ip: IndexPath = []
        ip.append(IndexPath(indexes: []))
        
        expectEqual(ip.count, 0)
    }
    
    func testAppendManyIndexPath() {
        var ip: IndexPath = []
        ip.append(IndexPath(indexes: [1, 2, 3]))
        
        expectEqual(ip.count, 3)
        expectEqual(ip[0], 1)
        expectEqual(ip[1], 2)
        expectEqual(ip[2], 3)
    }
    
    func testAppendEmptyIndexPathToSingle() {
        var ip: IndexPath = [1]
        ip.append(IndexPath(indexes: []))
        
        expectEqual(ip.count, 1)
        expectEqual(ip[0], 1)
    }
    
    func testAppendSingleIndexPath() {
        var ip: IndexPath = []
        ip.append(IndexPath(indexes: [1]))
        
        expectEqual(ip.count, 1)
        expectEqual(ip[0], 1)
    }
    
    func testAppendSingleIndexPathToSingle() {
        var ip: IndexPath = [1]
        ip.append(IndexPath(indexes: [1]))
        
        expectEqual(ip.count, 2)
        expectEqual(ip[0], 1)
        expectEqual(ip[1], 1)
    }
    
    func testAppendPairIndexPath() {
        var ip: IndexPath = []
        ip.append(IndexPath(indexes: [1, 2]))
        
        expectEqual(ip.count, 2)
        expectEqual(ip[0], 1)
        expectEqual(ip[1], 2)
    }
    
    func testAppendManyIndexPathToEmpty() {
        var ip: IndexPath = []
        ip.append(IndexPath(indexes: [1, 2, 3]))
        
        expectEqual(ip.count, 3)
        expectEqual(ip[0], 1)
        expectEqual(ip[1], 2)
        expectEqual(ip[2], 3)
    }
    
    func testAppendByOperator() {
        let ip1: IndexPath = []
        let ip2: IndexPath = []
        
        let ip3 = ip1 + ip2
        expectEqual(ip3.count, 0)
        
        let ip4: IndexPath = [1]
        let ip5: IndexPath = [2]
        
        let ip6 = ip4 + ip5
        expectEqual(ip6.count, 2)
        expectEqual(ip6[0], 1)
        expectEqual(ip6[1], 2)
        
        var ip7: IndexPath = []
        ip7 += ip6
        expectEqual(ip7.count, 2)
        expectEqual(ip7[0], 1)
        expectEqual(ip7[1], 2)
    }
    
    func testAppendArray() {
        var ip: IndexPath = [1, 2, 3, 4]
        let indexes = [5, 6, 7]
        
        ip.append(indexes)
        
        expectEqual(ip.count, 7)
        expectEqual(ip[0], 1)
        expectEqual(ip[6], 7)
    }
    
    func testRanges() {
        let ip1 = IndexPath(indexes: [1, 2, 3])
        let ip2 = IndexPath(indexes: [6, 7, 8])
        
        // Replace the whole range
        var mutateMe = ip1
        mutateMe[0..<3] = ip2
        expectEqual(mutateMe, ip2)
        
        // Insert at the beginning
        mutateMe = ip1
        mutateMe[0..<0] = ip2
        expectEqual(mutateMe, IndexPath(indexes: [6, 7, 8, 1, 2, 3]))
        
        // Insert at the end
        mutateMe = ip1
        mutateMe[3..<3] = ip2
        expectEqual(mutateMe, IndexPath(indexes: [1, 2, 3, 6, 7, 8]))
        
        // Insert in middle
        mutateMe = ip1
        mutateMe[2..<2] = ip2
        expectEqual(mutateMe, IndexPath(indexes: [1, 2, 6, 7, 8, 3]))
    }
    
    func testRangeFromEmpty() {
        let ip1 = IndexPath()
        let ip2 = ip1[0..<0]
        expectEqual(ip2.count, 0)
    }
    
    func testRangeFromSingle() {
        let ip1 = IndexPath(indexes: [1])
        let ip2 = ip1[0..<0]
        expectEqual(ip2.count, 0)
        let ip3 = ip1[0..<1]
        expectEqual(ip3.count, 1)
        expectEqual(ip3[0], 1)
    }
    
    func testRangeFromPair() {
        let ip1 = IndexPath(indexes: [1, 2])
        let ip2 = ip1[0..<0]
        expectEqual(ip2.count, 0)
        let ip3 = ip1[0..<1]
        expectEqual(ip3.count, 1)
        expectEqual(ip3[0], 1)
        let ip4 = ip1[1..<1]
        expectEqual(ip4.count, 0)
        let ip5 = ip1[0..<2]
        expectEqual(ip5.count, 2)
        expectEqual(ip5[0], 1)
        expectEqual(ip5[1], 2)
        let ip6 = ip1[1..<2]
        expectEqual(ip6.count, 1)
        expectEqual(ip6[0], 2)
        let ip7 = ip1[2..<2]
        expectEqual(ip7.count, 0)
    }
    
    func testRangeFromMany() {
        let ip1 = IndexPath(indexes: [1, 2, 3])
        let ip2 = ip1[0..<0]
        expectEqual(ip2.count, 0)
        let ip3 = ip1[0..<1]
        expectEqual(ip3.count, 1)
        let ip4 = ip1[0..<2]
        expectEqual(ip4.count, 2)
        let ip5 = ip1[0..<3]
        expectEqual(ip5.count, 3)
    }
    
    func testRangeReplacementSingle() {
        var ip1 = IndexPath(indexes: [1])
        ip1[0..<1] = IndexPath(indexes: [2])
        expectEqual(ip1[0], 2)
        
        ip1[0..<1] = IndexPath(indexes: [])
        expectEqual(ip1.count, 0)
    }
    
    func testRangeReplacementPair() {
        var ip1 = IndexPath(indexes: [1, 2])
        ip1[0..<1] = IndexPath(indexes: [2, 3])
        expectEqual(ip1.count, 3)
        expectEqual(ip1[0], 2)
        expectEqual(ip1[1], 3)
        expectEqual(ip1[2], 2)
        
        ip1[0..<1] = IndexPath(indexes: [])
        expectEqual(ip1.count, 2)
    }
    
    func testMoreRanges() {
        var ip = IndexPath(indexes: [1, 2, 3])
        let ip2 = IndexPath(indexes: [5, 6, 7, 8, 9, 10])
        
        ip[1..<2] = ip2
        expectEqual(ip, IndexPath(indexes: [1, 5, 6, 7, 8, 9, 10, 3]))
    }
    
    func testIteration() {
        let ip = IndexPath(indexes: [1, 2, 3])
        
        var count = 0
        for _ in ip {
            count += 1
        }
        
        expectEqual(3, count)
    }
    
    func testDescription() {
        let ip1: IndexPath = []
        let ip2: IndexPath = [1]
        let ip3: IndexPath = [1, 2]
        let ip4: IndexPath = [1, 2, 3]
        
        expectEqual(ip1.description, "[]")
        expectEqual(ip2.description, "[1]")
        expectEqual(ip3.description, "[1, 2]")
        expectEqual(ip4.description, "[1, 2, 3]")
        
        expectEqual(ip1.debugDescription, ip1.description)
        expectEqual(ip2.debugDescription, ip2.description)
        expectEqual(ip3.debugDescription, ip3.description)
        expectEqual(ip4.debugDescription, ip4.description)
    }
    
    func testBridgeToObjC() {
        let ip1: IndexPath = []
        let ip2: IndexPath = [1]
        let ip3: IndexPath = [1, 2]
        let ip4: IndexPath = [1, 2, 3]
        
        let nsip1 = ip1._bridgeToObjectiveC()
        let nsip2 = ip2._bridgeToObjectiveC()
        let nsip3 = ip3._bridgeToObjectiveC()
        let nsip4 = ip4._bridgeToObjectiveC()
        
        expectEqual(nsip1.length, 0)
        expectEqual(nsip2.length, 1)
        expectEqual(nsip3.length, 2)
        expectEqual(nsip4.length, 3)
    }
    
    func testForceBridgeFromObjC() {
        let nsip1 = NSIndexPath()
        let nsip2 = NSIndexPath(index: 1)
        let nsip3 = [1, 2].withUnsafeBufferPointer { (buffer: UnsafeBufferPointer<Int>) -> NSIndexPath in
            return NSIndexPath(indexes: buffer.baseAddress, length: buffer.count)
        }
        let nsip4 = [1, 2, 3].withUnsafeBufferPointer { (buffer: UnsafeBufferPointer<Int>) -> NSIndexPath in
            return NSIndexPath(indexes: buffer.baseAddress, length: buffer.count)
        }
        
        var ip1: IndexPath? = IndexPath()
        IndexPath._forceBridgeFromObjectiveC(nsip1, result: &ip1)
        expectNotNil(ip1)
        expectEqual(ip1!.count, 0)
        
        var ip2: IndexPath? = IndexPath()
        IndexPath._forceBridgeFromObjectiveC(nsip2, result: &ip2)
        expectNotNil(ip2)
        expectEqual(ip2!.count, 1)
        expectEqual(ip2![0], 1)
        
        var ip3: IndexPath? = IndexPath()
        IndexPath._forceBridgeFromObjectiveC(nsip3, result: &ip3)
        expectNotNil(ip3)
        expectEqual(ip3!.count, 2)
        expectEqual(ip3![0], 1)
        expectEqual(ip3![1], 2)
        
        var ip4: IndexPath? = IndexPath()
        IndexPath._forceBridgeFromObjectiveC(nsip4, result: &ip4)
        expectNotNil(ip4)
        expectEqual(ip4!.count, 3)
        expectEqual(ip4![0], 1)
        expectEqual(ip4![1], 2)
        expectEqual(ip4![2], 3)
    }
    
    func testConditionalBridgeFromObjC() {
        let nsip1 = NSIndexPath()
        let nsip2 = NSIndexPath(index: 1)
        let nsip3 = [1, 2].withUnsafeBufferPointer { (buffer: UnsafeBufferPointer<Int>) -> NSIndexPath in
            return NSIndexPath(indexes: buffer.baseAddress, length: buffer.count)
        }
        let nsip4 = [1, 2, 3].withUnsafeBufferPointer { (buffer: UnsafeBufferPointer<Int>) -> NSIndexPath in
            return NSIndexPath(indexes: buffer.baseAddress, length: buffer.count)
        }
        
        var ip1: IndexPath? = IndexPath()
        expectTrue(IndexPath._conditionallyBridgeFromObjectiveC(nsip1, result: &ip1))
        expectNotNil(ip1)
        expectEqual(ip1!.count, 0)
        
        var ip2: IndexPath? = IndexPath()
        expectTrue(IndexPath._conditionallyBridgeFromObjectiveC(nsip2, result: &ip2))
        expectNotNil(ip2)
        expectEqual(ip2!.count, 1)
        expectEqual(ip2![0], 1)
        
        var ip3: IndexPath? = IndexPath()
        expectTrue(IndexPath._conditionallyBridgeFromObjectiveC(nsip3, result: &ip3))
        expectNotNil(ip3)
        expectEqual(ip3!.count, 2)
        expectEqual(ip3![0], 1)
        expectEqual(ip3![1], 2)
        
        var ip4: IndexPath? = IndexPath()
        expectTrue(IndexPath._conditionallyBridgeFromObjectiveC(nsip4, result: &ip4))
        expectNotNil(ip4)
        expectEqual(ip4!.count, 3)
        expectEqual(ip4![0], 1)
        expectEqual(ip4![1], 2)
        expectEqual(ip4![2], 3)
    }
    
    func testUnconditionalBridgeFromObjC() {
        let nsip1 = NSIndexPath()
        let nsip2 = NSIndexPath(index: 1)
        let nsip3 = [1, 2].withUnsafeBufferPointer { (buffer: UnsafeBufferPointer<Int>) -> NSIndexPath in
            return NSIndexPath(indexes: buffer.baseAddress, length: buffer.count)
        }
        let nsip4 = [1, 2, 3].withUnsafeBufferPointer { (buffer: UnsafeBufferPointer<Int>) -> NSIndexPath in
            return NSIndexPath(indexes: buffer.baseAddress, length: buffer.count)
        }
        
        let ip1: IndexPath = IndexPath._unconditionallyBridgeFromObjectiveC(nsip1)
        expectEqual(ip1.count, 0)
        
        var ip2: IndexPath = IndexPath._unconditionallyBridgeFromObjectiveC(nsip2)
        expectEqual(ip2.count, 1)
        expectEqual(ip2[0], 1)
        
        var ip3: IndexPath = IndexPath._unconditionallyBridgeFromObjectiveC(nsip3)
        expectEqual(ip3.count, 2)
        expectEqual(ip3[0], 1)
        expectEqual(ip3[1], 2)
        
        var ip4: IndexPath = IndexPath._unconditionallyBridgeFromObjectiveC(nsip4)
        expectEqual(ip4.count, 3)
        expectEqual(ip4[0], 1)
        expectEqual(ip4[1], 2)
        expectEqual(ip4[2], 3)
    }
    
    func testObjcBridgeType() {
        expectTrue(IndexPath._getObjectiveCType() == NSIndexPath.self)
    }
    
    func test_AnyHashableContainingIndexPath() {
        let values: [IndexPath] = [
            IndexPath(indexes: [1, 2]),
            IndexPath(indexes: [1, 2, 3]),
            IndexPath(indexes: [1, 2, 3]),
            ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(IndexPath.self, type(of: anyHashables[0].base))
        expectEqual(IndexPath.self, type(of: anyHashables[1].base))
        expectEqual(IndexPath.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSIndexPath() {
        let values: [NSIndexPath] = [
            NSIndexPath(index: 1),
            NSIndexPath(index: 2),
            NSIndexPath(index: 2),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(IndexPath.self, type(of: anyHashables[0].base))
        expectEqual(IndexPath.self, type(of: anyHashables[1].base))
        expectEqual(IndexPath.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_unconditionallyBridgeFromObjectiveC() {
        expectEqual(IndexPath(), IndexPath._unconditionallyBridgeFromObjectiveC(nil))
    }

    func test_slice_1ary() {
        let indexPath: IndexPath = [0]
        let res = indexPath.dropFirst()
        expectEqual(0, res.count)

        let slice = indexPath[1..<1]
        expectEqual(0, slice.count)
    }

    func test_dropFirst() {
        var pth = IndexPath(indexes:[1,2,3,4])
        while !pth.isEmpty {
            // this should not crash 
            pth = pth.dropFirst()
        }
    }
}

#if !FOUNDATION_XCTEST
var IndexPathTests = TestSuite("TestIndexPath")
IndexPathTests.test("testEmpty") { TestIndexPath().testEmpty() }
IndexPathTests.test("testSingleIndex") { TestIndexPath().testSingleIndex() }
IndexPathTests.test("testTwoIndexes") { TestIndexPath().testTwoIndexes() }
IndexPathTests.test("testManyIndexes") { TestIndexPath().testManyIndexes() }
IndexPathTests.test("testCreateFromSequence") { TestIndexPath().testCreateFromSequence() }
IndexPathTests.test("testCreateFromLiteral") { TestIndexPath().testCreateFromLiteral() }
IndexPathTests.test("testDropLast") { TestIndexPath().testDropLast() }
IndexPathTests.test("testDropLastFromEmpty") { TestIndexPath().testDropLastFromEmpty() }
IndexPathTests.test("testDropLastFromSingle") { TestIndexPath().testDropLastFromSingle() }
IndexPathTests.test("testDropLastFromPair") { TestIndexPath().testDropLastFromPair() }
IndexPathTests.test("testDropLastFromTriple") { TestIndexPath().testDropLastFromTriple() }
IndexPathTests.test("testStartEndIndex") { TestIndexPath().testStartEndIndex() }
IndexPathTests.test("testIterator") { TestIndexPath().testIterator() }
IndexPathTests.test("testIndexing") { TestIndexPath().testIndexing() }
IndexPathTests.test("testCompare") { TestIndexPath().testCompare() }
IndexPathTests.test("testHashing") { TestIndexPath().testHashing() }
IndexPathTests.test("testEquality") { TestIndexPath().testEquality() }
IndexPathTests.test("testSubscripting") { TestIndexPath().testSubscripting() }
IndexPathTests.test("testAppending") { TestIndexPath().testAppending() }
IndexPathTests.test("testAppendEmpty") { TestIndexPath().testAppendEmpty() }
IndexPathTests.test("testAppendEmptyIndexPath") { TestIndexPath().testAppendEmptyIndexPath() }
IndexPathTests.test("testAppendManyIndexPath") { TestIndexPath().testAppendManyIndexPath() }
IndexPathTests.test("testAppendEmptyIndexPathToSingle") { TestIndexPath().testAppendEmptyIndexPathToSingle() }
IndexPathTests.test("testAppendSingleIndexPath") { TestIndexPath().testAppendSingleIndexPath() }
IndexPathTests.test("testAppendSingleIndexPathToSingle") { TestIndexPath().testAppendSingleIndexPathToSingle() }
IndexPathTests.test("testAppendPairIndexPath") { TestIndexPath().testAppendPairIndexPath() }
IndexPathTests.test("testAppendManyIndexPathToEmpty") { TestIndexPath().testAppendManyIndexPathToEmpty() }
IndexPathTests.test("testAppendByOperator") { TestIndexPath().testAppendByOperator() }
IndexPathTests.test("testAppendArray") { TestIndexPath().testAppendArray() }
IndexPathTests.test("testRanges") { TestIndexPath().testRanges() }
IndexPathTests.test("testRangeFromEmpty") { TestIndexPath().testRangeFromEmpty() }
IndexPathTests.test("testRangeFromSingle") { TestIndexPath().testRangeFromSingle() }
IndexPathTests.test("testRangeFromPair") { TestIndexPath().testRangeFromPair() }
IndexPathTests.test("testRangeFromMany") { TestIndexPath().testRangeFromMany() }
IndexPathTests.test("testRangeReplacementSingle") { TestIndexPath().testRangeReplacementSingle() }
IndexPathTests.test("testRangeReplacementPair") { TestIndexPath().testRangeReplacementPair() }
IndexPathTests.test("testMoreRanges") { TestIndexPath().testMoreRanges() }
IndexPathTests.test("testIteration") { TestIndexPath().testIteration() }
IndexPathTests.test("testDescription") { TestIndexPath().testDescription() }
IndexPathTests.test("testBridgeToObjC") { TestIndexPath().testBridgeToObjC() }
IndexPathTests.test("testForceBridgeFromObjC") { TestIndexPath().testForceBridgeFromObjC() }
IndexPathTests.test("testConditionalBridgeFromObjC") { TestIndexPath().testConditionalBridgeFromObjC() }
IndexPathTests.test("testUnconditionalBridgeFromObjC") { TestIndexPath().testUnconditionalBridgeFromObjC() }
IndexPathTests.test("testObjcBridgeType") { TestIndexPath().testObjcBridgeType() }
IndexPathTests.test("test_AnyHashableContainingIndexPath") { TestIndexPath().test_AnyHashableContainingIndexPath() }
IndexPathTests.test("test_AnyHashableCreatedFromNSIndexPath") { TestIndexPath().test_AnyHashableCreatedFromNSIndexPath() }
IndexPathTests.test("test_unconditionallyBridgeFromObjectiveC") { TestIndexPath().test_unconditionallyBridgeFromObjectiveC() }
IndexPathTests.test("test_slice_1ary") { TestIndexPath().test_slice_1ary() }
IndexPathTests.test("test_dropFirst") { TestIndexPath().test_dropFirst() }
runAllTests()
#endif
