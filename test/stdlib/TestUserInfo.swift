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
class TestUserInfoSuper : XCTestCase { }
#else
import StdlibUnittest
class TestUserInfoSuper : NSObject { }
#endif

struct SubStruct: Equatable {
    var i: Int
    var str: String

    static func ==(lhs: SubStruct, rhs: SubStruct) -> Bool {
        return lhs.i == rhs.i && 
               lhs.str == rhs.str
    }
}

struct SomeStructure: Hashable {
    var i: Int
    var str: String
    var sub: SubStruct

    static func ==(lhs: SomeStructure, rhs: SomeStructure) -> Bool {
        return lhs.i == rhs.i && 
               lhs.str == rhs.str && 
               lhs.sub == rhs.sub
    }

    // FIXME: we don't care about this, but Any only finds == on Hashables
    var hashValue: Int { return i }
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
        let info: [AnyHashable : Any] = [
            AnyHashable(userInfoKey) : testStructure
        ]
        let note = Notification(name: notifName, userInfo: info)
        expectNotNil(note.userInfo)
        let nc = NotificationCenter.default
        nc.addObserver(self, selector: #selector(TestUserInfo.notification(_:)), name: notifName, object: nil)
        nc.post(note)
        expectNotNil(posted)
        if let notification = posted {
            let postedInfo = notification.userInfo
            expectNotNil(postedInfo)
            if let userInfo = postedInfo {
                let postedValue = userInfo[AnyHashable(userInfoKey)] as? SomeStructure
                expectNotNil(postedValue)
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
        let testStructure2 = SomeStructure(i: 6, str: "10", sub: SubStruct(i: 6, str: "11"))
        let info1: [AnyHashable : Any] = [
            AnyHashable(userInfoKey) : testStructure
        ]
        let info2: [AnyHashable : Any] = [
            AnyHashable(userInfoKey) : "this can convert"
        ]
        let info3: [AnyHashable : Any] = [
            AnyHashable(userInfoKey) : testStructure2
        ]

        let note1 = Notification(name: notifName, userInfo: info1)
        let note2 = Notification(name: notifName, userInfo: info1)
        expectEqual(note1, note2)

        let note3 = Notification(name: notifName, userInfo: info2)
        let note4 = Notification(name: notifName, userInfo: info2)
        expectEqual(note3, note4)

        let note5 = Notification(name: notifName, userInfo: info3)
        expectNotEqual(note1, note5)
    }

    @objc func notification(_ notif: Notification) {
        posted = notif
    }

    // MARK: -
    func test_classForCoder() {
        // confirm internal bridged impl types are not exposed to archival machinery
        // we have to be circuitous here, as bridging makes it very difficult to confirm this
        let note = Notification(name: Notification.Name(rawValue: "TestSwiftNotification"), userInfo: [AnyHashable("key"):"value"])
        let archivedNote = NSKeyedArchiver.archivedData(withRootObject: note)
        let noteAsPlist = try! PropertyListSerialization.propertyList(from: archivedNote, options: [], format: nil)
        let plistAsData = try! PropertyListSerialization.data(fromPropertyList: noteAsPlist, format: .xml, options: 0)
        let xml = NSString(data: plistAsData, encoding: String.Encoding.utf8.rawValue)!
        expectEqual(xml.range(of: "_NSUserInfoDictionary").location, NSNotFound)
    }

    func test_AnyHashableContainingNotification() {
        let values: [Notification] = [
            Notification(name: Notification.Name(rawValue: "TestSwiftNotification")),
            Notification(name: Notification.Name(rawValue: "TestOtherSwiftNotification")),
            Notification(name: Notification.Name(rawValue: "TestOtherSwiftNotification")),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Notification.self, type(of: anyHashables[0].base))
        expectEqual(Notification.self, type(of: anyHashables[1].base))
        expectEqual(Notification.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSNotification() {
        let values: [NSNotification] = [
            NSNotification(name: Notification.Name(rawValue: "TestSwiftNotification"), object: nil),
            NSNotification(name: Notification.Name(rawValue: "TestOtherSwiftNotification"), object: nil),
            NSNotification(name: Notification.Name(rawValue: "TestOtherSwiftNotification"), object: nil),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Notification.self, type(of: anyHashables[0].base))
        expectEqual(Notification.self, type(of: anyHashables[1].base))
        expectEqual(Notification.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }
}

#if !FOUNDATION_XCTEST
var UserInfoTests = TestSuite("UserInfo")
UserInfoTests.test("test_userInfoPost") { TestUserInfo().test_userInfoPost() }
UserInfoTests.test("test_equality") { TestUserInfo().test_equality() }
UserInfoTests.test("test_classForCoder") { TestUserInfo().test_classForCoder() }
UserInfoTests.test("test_AnyHashableContainingNotification") { TestUserInfo().test_AnyHashableContainingNotification() }
UserInfoTests.test("test_AnyHashableCreatedFromNSNotification") { TestUserInfo().test_AnyHashableCreatedFromNSNotification() }
runAllTests()
#endif
