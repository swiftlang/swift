// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

class TestIndexPath : TestIndexPathSuper {
    
    func testBasics() {
        let ip = IndexPath(index: 1)
        expectEqual(ip.count, 1)
    }
    
    func testAppending() {
        var ip : IndexPath = [1, 2, 3, 4]
        let ip2 = IndexPath(indexes: [5, 6, 7])
        
        ip.append(ip2)
        
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
    
    // TODO: Test bridging
    
}

#if !FOUNDATION_XCTEST
var IndexPathTests = TestSuite("TestIndexPath")
IndexPathTests.test("testBasics") { TestIndexPath().testBasics() }
IndexPathTests.test("testAppending") { TestIndexPath().testAppending() }
IndexPathTests.test("testRanges") { TestIndexPath().testRanges() }
IndexPathTests.test("testMoreRanges") { TestIndexPath().testMoreRanges() }
IndexPathTests.test("testIteration") { TestIndexPath().testIteration() }
IndexPathTests.test("test_AnyHashableContainingIndexPath") { TestIndexPath().test_AnyHashableContainingIndexPath() }
IndexPathTests.test("test_AnyHashableCreatedFromNSIndexPath") { TestIndexPath().test_AnyHashableCreatedFromNSIndexPath() }
runAllTests()
#endif

