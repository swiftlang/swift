// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang %S/Inputs/FoundationBridge/FoundationBridge.m -c -o %t/FoundationBridgeObjC.o -g
// RUN: %target-build-swift %s -I %S/Inputs/FoundationBridge/ -Xlinker %t/FoundationBridgeObjC.o -o %t/TestNSStringBridge

// RUN: %target-run %t/TestNSStringBridge > %t.txt
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import FoundationBridgeObjC

#if FOUNDATION_XCTEST
    import XCTest
    class TestNSStringBridgeSuper : XCTestCase { }
#else
    import StdlibUnittest
    class TestNSStringBridgeSuper { }
#endif


// these are just here so that AnyObject picks up the SPI for Foundation fast-path access in ObjC
class SelectorPlaceholder : NSObject {
    @objc(_fastCharacterContents)
    func _fastCharacterContents() -> UnsafePointer<unichar>? { return nil }
    @objc(_fastCStringContents:)
    func _fastCStringContents(_ nullTerminationRequired: Bool) -> UnsafePointer<Int8>? { return nil }
}

class TestNSStringBridge : TestNSStringBridgeSuper {
    func test_bridgeASCII() {
        let string = "hello world"
        let fastCharacters = (string as AnyObject)._fastCharacterContents()
        let fastCStringNullTerm = (string as AnyObject)._fastCStringContents(true)
        let fastCString = (string as AnyObject)._fastCStringContents(false)
        expectNil(fastCharacters)
        expectNotNil(fastCStringNullTerm)
        expectNotNil(fastCString)
        expectEqual(string.fastestEncoding, String.Encoding.ascii)
        expectEqual(string.smallestEncoding, String.Encoding.ascii)
    }

    func test_bridgeUnicode() {
        let string = "héllö wørld"
        let fastCharacters = (string as AnyObject)._fastCharacterContents()
        let fastCStringNullTerm = (string as AnyObject)._fastCStringContents(true)
        let fastCString = (string as AnyObject)._fastCStringContents(false)
        expectNotNil(fastCharacters)
        expectNil(fastCStringNullTerm)
        expectNil(fastCString)
        expectEqual(string.fastestEncoding, String.Encoding.utf16)
        expectEqual(string.smallestEncoding, String.Encoding.utf16)
    }

    func test_copy() {
        let string1 = "test"
        let ns1 = string1 as NSString
        expectTrue(ObjectBehaviorVerifier.verifyEquality(of: ns1, with: ns1, options: [.copy, .rawPointerCompare]))

        let string2 = "This is a test of strings"
        let ns2 = string2 as NSString
        expectTrue(ObjectBehaviorVerifier.verifyEquality(of: ns2, with: ns2, options: [.copy, .rawPointerCompare]))


        let string3 = "héllö wørld"
        let ns3 = string3 as NSString
        expectTrue(ObjectBehaviorVerifier.verifyEquality(of: ns3, with: ns3, options: [.copy, .rawPointerCompare]))
    }
}

#if !FOUNDATION_XCTEST
var NSStringBridgeTests = TestSuite("TestNSStringBridge")
NSStringBridgeTests.test("test_bridgeASCII") { TestNSStringBridge().test_bridgeASCII() }
NSStringBridgeTests.test("test_bridgeUnicode") { TestNSStringBridge().test_bridgeUnicode() }
NSStringBridgeTests.test("test_copy") { TestNSStringBridge().test_copy() }
runAllTests()
#endif
