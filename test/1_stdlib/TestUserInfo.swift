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
class TestUserInfoSuper : XCTestCase { }
#else
import StdlibUnittest
class TestUserInfoSuper : NSObject { }
#endif

struct SubStruct {
    var i: Int
    var str: String
}

struct SomeStructure {
    var i: Int
    var str: String
    var sub: SubStruct
}

/*
 Notification and potentially other structures require a representation of a 
 userInfo dictionary. The Objective-C counterparts are represented via
 NSDictionary which can only store a hashable key (actually 
 NSObject<NSCopying> *) and a value of AnyObject (actually NSObject *). However
 it is desired in swift to store Any in the value. These structure expositions
 in swift have an adapter that allows them to pass a specialized NSDictionary
 subclass to the Objective-C layer that can round trip the stored Any types back
 out into Swift.

 In this case NSNotification -> Notification bridging is suitable to verify that
 behavior.
*/

class TestUserInfo : TestUserInfoSuper {
    var posted: Notification?

    func validate(_ testStructure: SomeStructure, _ value: SomeStructure) {
        expectEqual(testStructure.i, value.i)
        expectEqual(testStructure.str, value.str)
        expectEqual(testStructure.sub.i, value.sub.i)
        expectEqual(testStructure.sub.str, value.sub.str)
    }

    func test_userInfoPost() {
        let userInfoKey = "userInfoKey"
        let notifName = Notification.Name(rawValue: "TestSwiftNotification")
        let testStructure = SomeStructure(i: 5, str: "10", sub: SubStruct(i: 6, str: "11"))
        let info: [String : Any] = [
            userInfoKey : testStructure
        ]
        let note = Notification(name: notifName, userInfo: info)
        expectNotEmpty(note.userInfo)
        let nc = NotificationCenter.`default`()
        nc.addObserver(self, selector: #selector(TestUserInfo.notification(_:)), name: notifName, object: nil)
        nc.post(note)
        expectNotEmpty(posted)
        if let notification = posted {
            let postedInfo = notification.userInfo
            expectNotEmpty(postedInfo)
            if let userInfo = postedInfo {
                let postedValue = userInfo[userInfoKey] as? SomeStructure
                expectNotEmpty(postedValue)
                if let value = postedValue {
                    validate(testStructure, value)
                }
            }
        }
    }

    func test_equality() {
        let userInfoKey = "userInfoKey"
        let notifName = Notification.Name(rawValue: "TestSwiftNotification")
        let testStructure = SomeStructure(i: 5, str: "10", sub: SubStruct(i: 6, str: "11"))
        let info1: [String : Any] = [
            userInfoKey : testStructure
        ]
        let info2: [String : Any] = [
            userInfoKey : "this can convert"
        ]

        let note1 = Notification(name: notifName, userInfo: info1)
        let note2 = Notification(name: notifName, userInfo: info1)
        expectNotEqual(note1, note2)

        let note3 = Notification(name: notifName, userInfo: info2)
        let note4 = Notification(name: notifName, userInfo: info2)
        expectEqual(note3, note4)
    }

    func notification(_ notif: Notification) {
        posted = notif
    }
}

#if !FOUNDATION_XCTEST
var UserInfoTests = TestSuite("UserInfo")
UserInfoTests.test("test_userInfoPost") { TestUserInfo().test_userInfoPost() }
UserInfoTests.test("test_equality") { TestUserInfo().test_equality() }
runAllTests()
#endif
