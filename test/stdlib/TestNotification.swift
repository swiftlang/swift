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
// RUN: %target-codesign %t/TestNotification

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

    func test_hashing() {
        guard #available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *) else { return }

        let o1 = NSObject()
        let o2 = NSObject()
        let values: [Notification] = [
            /* 0 */ Notification(name: .init("a"), object: o1, userInfo: nil),
            /* 1 */ Notification(name: .init("a"), object: o2, userInfo: nil),
            /* 2 */ Notification(name: .init("b"), object: o1, userInfo: nil),
            /* 3 */ Notification(name: .init("b"), object: o2, userInfo: nil),
            /* 4 */ Notification(name: .init("a"), object: o1, userInfo: ["Foo": 1]),
            /* 5 */ Notification(name: .init("a"), object: o1, userInfo: ["Foo": 2]),
            /* 6 */ Notification(name: .init("a"), object: o1, userInfo: ["Bar": 1]),
            /* 7 */ Notification(name: .init("a"), object: o1, userInfo: ["Foo": 1, "Bar": 2]),
        ]

        let hashGroups: [Int: Int] = [
            0: 0,
            1: 0,
            2: 1,
            3: 1,
            4: 2,
            5: 2,
            6: 3,
            7: 4
        ]

        checkHashable(
            values,
            equalityOracle: { $0 == $1 },
            hashEqualityOracle: {
                // FIXME: Unfortunately while we have 8 different notifications,
                // three pairs of them have colliding hash encodings.
                hashGroups[$0] == hashGroups[$1]
            })
    }
}


#if !FOUNDATION_XCTEST
var NotificationTests = TestSuite("TestNotification")
NotificationTests.test("test_unconditionallyBridgeFromObjectiveC") { TestNotification().test_unconditionallyBridgeFromObjectiveC() }
NotificationTests.test("test_hashing") { TestNotification().test_hashing() }

private struct NonHashableValueType: Equatable {
    let value: Int
    init(_ value: Int) {
        self.value = value
    }
}

NotificationTests.test("test_reflexivity_violation")
  .xfail(
    .custom({ true },
        reason: "<rdar://problem/49797185> Foundation.Notification's equality relation isn't reflexive"))
  .code {
    let name = Notification.Name("name")
    let a = NonHashableValueType(1)
    let b = NonHashableValueType(2)
    // Currently none of these values compare equal to themselves:
    let values: [Notification] = [
        Notification(name: name, object: a, userInfo: nil),
        Notification(name: name, object: b, userInfo: nil),
        Notification(name: name, object: nil, userInfo: ["foo": a]),
        Notification(name: name, object: nil, userInfo: ["foo": b]),
    ]
    checkHashable(values, equalityOracle: { $0 == $1 })
}


runAllTests()
#endif
