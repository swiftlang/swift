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
// RUN: pushd %t
// RUN: %target-build-swift -emit-module -emit-library %S/Inputs/FoundationTestsShared.swift -module-name FoundationTestsShared -module-link-name FoundationTestsShared
// RUN: popd %t
//
// RUN: %target-build-swift %s -I%t -L%t -o %t/TestPersonNameComponents
// RUN: %target-run %t/TestPersonNameComponents
//
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import FoundationTestsShared
import CoreFoundation

#if FOUNDATION_XCTEST
import XCTest
class TestPersonNameComponentsSuper : XCTestCase { }
#else
import StdlibUnittest
class TestPersonNameComponentsSuper { }
#endif

class TestPersonNameComponents : TestPersonNameComponentsSuper {
    @available(OSX 10.11, iOS 9.0, *)
    func makePersonNameComponents(namePrefix: String? = nil, givenName: String, middleName: String? = nil, familyName: String, nameSuffix: String? = nil, nickname: String? = nil) -> PersonNameComponents {
        var result = PersonNameComponents()
        result.namePrefix = namePrefix
        result.givenName = givenName
        result.middleName = middleName
        result.familyName = familyName
        result.nameSuffix = nameSuffix
        result.nickname = nickname
        return result
    }
    func test_AnyHashableContainingPersonNameComponents() {
        if #available(OSX 10.11, iOS 9.0, *) {
            let values: [PersonNameComponents] = [
                makePersonNameComponents(givenName: "Kevin", familyName: "Frank"),
                makePersonNameComponents(givenName: "John", familyName: "Appleseed"),
                makePersonNameComponents(givenName: "John", familyName: "Appleseed"),
            ]
            let anyHashables = values.map(AnyHashable.init)
            expectEqual(PersonNameComponents.self, type(of: anyHashables[0].base))
            expectEqual(PersonNameComponents.self, type(of: anyHashables[1].base))
            expectEqual(PersonNameComponents.self, type(of: anyHashables[2].base))
            expectNotEqual(anyHashables[0], anyHashables[1])
            expectEqual(anyHashables[1], anyHashables[2])
        }
    }

    @available(OSX 10.11, iOS 9.0, *)
    func makeNSPersonNameComponents(givenName: String, familyName: String) -> NSPersonNameComponents {
        let result = NSPersonNameComponents()
        result.givenName = givenName
        result.familyName = familyName
        return result
    }

    func test_AnyHashableCreatedFromNSPersonNameComponents() {
        if #available(OSX 10.11, iOS 9.0, *) {
            let values: [NSPersonNameComponents] = [
                makeNSPersonNameComponents(givenName: "Kevin", familyName: "Frank"),
                makeNSPersonNameComponents(givenName: "John", familyName: "Appleseed"),
                makeNSPersonNameComponents(givenName: "John", familyName: "Appleseed"),
            ]
            let anyHashables = values.map(AnyHashable.init)
            expectEqual(PersonNameComponents.self, type(of: anyHashables[0].base))
            expectEqual(PersonNameComponents.self, type(of: anyHashables[1].base))
            expectEqual(PersonNameComponents.self, type(of: anyHashables[2].base))
            expectNotEqual(anyHashables[0], anyHashables[1])
            expectEqual(anyHashables[1], anyHashables[2])
        }
    }

    func test_EncodingRoundTrip_JSON() {
        if #available(OSX 10.11, iOS 9.0, *) {
            let values: [PersonNameComponents] = [
                makePersonNameComponents(givenName: "John", familyName: "Appleseed"),
                makePersonNameComponents(givenName: "John", familyName: "Appleseed", nickname: "Johnny"),
                makePersonNameComponents(namePrefix: "Dr.", givenName: "Jane", middleName: "A.", familyName: "Appleseed", nameSuffix: "Esq.", nickname: "Janie")
            ]

            for components in values {
                expectRoundTripEqualityThroughJSON(for: components)
            }
        }
    }

    func test_EncodingRoundTrip_Plist() {
        if #available(OSX 10.11, iOS 9.0, *) {
            let values: [PersonNameComponents] = [
                makePersonNameComponents(givenName: "John", familyName: "Appleseed"),
                makePersonNameComponents(givenName: "John", familyName: "Appleseed", nickname: "Johnny"),
                makePersonNameComponents(namePrefix: "Dr.", givenName: "Jane", middleName: "A.", familyName: "Appleseed", nameSuffix: "Esq.", nickname: "Janie")
            ]

            for components in values {
                expectRoundTripEqualityThroughPlist(for: components)
            }
        }
    }
}

#if !FOUNDATION_XCTEST
var PersonNameComponentsTests = TestSuite("TestPersonNameComponents")
PersonNameComponentsTests.test("test_AnyHashableContainingPersonNameComponents") { TestPersonNameComponents().test_AnyHashableContainingPersonNameComponents() }
PersonNameComponentsTests.test("test_AnyHashableCreatedFromNSPersonNameComponents") { TestPersonNameComponents().test_AnyHashableCreatedFromNSPersonNameComponents() }
PersonNameComponentsTests.test("test_EncodingRoundTrip_JSON") { TestPersonNameComponents().test_EncodingRoundTrip_JSON() }
PersonNameComponentsTests.test("test_EncodingRoundTrip_Plist") { TestPersonNameComponents().test_EncodingRoundTrip_Plist() }
runAllTests()
#endif
