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
// RUN: %target-build-swift %s -I %S/Inputs/FoundationBridge/ -Xlinker %t/FoundationBridgeObjC.o -o %t/TestNotification

// RUN: %target-run %t/TestNotification > %t.txt
// REQUIRES: executable_test
// REQUIRES: objc_interop


import Foundation
import FoundationBridgeObjC

#if FOUNDATION_XCTEST
    import XCTest
    class TestNotificationSuper : XCTestCase { }
#else
    import StdlibUnittest
    class TestNotificationSuper { }
#endif

class TestNotification : TestNotificationSuper {
    func test_unconditionallyBridgeFromObjectiveC() {
        expectEqual(Notification(name: Notification.Name("")), Notification._unconditionallyBridgeFromObjectiveC(nil))
    }
}


#if !FOUNDATION_XCTEST
var NotificationTests = TestSuite("TestNotification")
NotificationTests.test("test_unconditionallyBridgeFromObjectiveC") { TestNotification().test_unconditionallyBridgeFromObjectiveC() }
runAllTests()
#endif
