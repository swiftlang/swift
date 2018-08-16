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
    class TestProgressSuper : XCTestCase { }
#else
    import StdlibUnittest
    class TestProgressSuper { }
#endif

class TestProgress : TestProgressSuper {
    func testUserInfoConveniences() {
        if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
            let p = Progress(parent:nil, userInfo: nil)
            
            expectNil(p.userInfo[.throughputKey])
            expectNil(p.throughput)
            p.throughput = 50
            expectEqual(p.throughput, 50)
            expectNotNil(p.userInfo[.throughputKey])
            
            expectNil(p.userInfo[.estimatedTimeRemainingKey])
            expectNil(p.estimatedTimeRemaining)
            p.estimatedTimeRemaining = 100
            expectEqual(p.estimatedTimeRemaining, 100)
            expectNotNil(p.userInfo[.estimatedTimeRemainingKey])
            
            expectNil(p.userInfo[.fileTotalCountKey])
            expectNil(p.fileTotalCount)
            p.fileTotalCount = 42
            expectEqual(p.fileTotalCount, 42)
            expectNotNil(p.userInfo[.fileTotalCountKey])
            
            expectNil(p.userInfo[.fileCompletedCountKey])
            expectNil(p.fileCompletedCount)
            p.fileCompletedCount = 24
            expectEqual(p.fileCompletedCount, 24)
            expectNotNil(p.userInfo[.fileCompletedCountKey])
        }
    }
    
    func testPerformAsCurrent() {
        if #available(OSX 10.11, iOS 8.0, *) {
            // This test can be enabled once <rdar://problem/31867347> is in the SDK
            /*
            let p = Progress.discreteProgress(totalUnitCount: 10)
            let r = p.performAsCurrent(withPendingUnitCount: 10) {
                expectNotNil(Progress.current())
                return 42
            }
            expectEqual(r, 42)
            expectEqual(p.completedUnitCount, 10)
            expectNil(Progress.current())
            */
        }
    }
}

#if !FOUNDATION_XCTEST
let ProgressTests = TestSuite("TestProgress")
ProgressTests.test("testUserInfoConveniences") { TestProgress().testUserInfoConveniences() }
ProgressTests.test("testPerformAsCurrent") { TestProgress().testPerformAsCurrent() }
runAllTests()
#endif
