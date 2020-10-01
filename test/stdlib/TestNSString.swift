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
// RUN: %target-build-swift %s -I %S/Inputs/FoundationBridge/ -Xlinker %t/FoundationBridgeObjC.o -sanitize=address -o %t/TestNSString
// RUN: %target-codesign %t/TestNSString

// RUN: %target-run %t/TestNSString > %t.txt
// REQUIRES: executable_test
// REQUIRES: asan_runtime
// REQUIRES: objc_interop
// REQUIRES: rdar55727144

import Foundation
import FoundationBridgeObjC

#if FOUNDATION_XCTEST
    import XCTest
    class TestNSStringSuper : XCTestCase { }
#else
    import StdlibUnittest
    class TestNSStringSuper { }
#endif

class TestNSString : TestNSStringSuper {
    
  func test_equalOverflow() {
    let cyrillic = "чебурашка@ящик-с-апельсинами.рф"
    let other = getNSStringEqualTestString()
    print(NSStringBridgeTestEqual(cyrillic, other))
  }
  
  func test_smallString_BOM() {
    let bom = "\u{FEFF}" // U+FEFF (ZERO WIDTH NO-BREAK SPACE)
//    expectEqual(1, NSString(string: bom).length)
//    expectEqual(4, NSString(string: "\(bom)abc").length)
//    expectEqual(5, NSString(string: "\(bom)\(bom)abc").length)
//    expectEqual(4, NSString(string: "a\(bom)bc").length)
//    expectEqual(13, NSString(string: "\(bom)234567890123").length)
//    expectEqual(14, NSString(string: "\(bom)2345678901234").length)
    
    expectEqual(1, (bom as NSString).length)
    expectEqual(4, ("\(bom)abc" as NSString).length)
    expectEqual(5, ("\(bom)\(bom)abc" as NSString).length)
    expectEqual(4, ("a\(bom)bc" as NSString).length)
    expectEqual(13, ("\(bom)234567890123" as NSString).length)
    expectEqual(14, ("\(bom)2345678901234" as NSString).length)
    
    let string = "\(bom)abc"
    let middleIndex = string.index(string.startIndex, offsetBy: 2)
    string.enumerateSubstrings(in: middleIndex..<string.endIndex, options: .byLines) { (_, _, _, _) in }  //shouldn't crash
  }
  
  func test_unpairedSurrogates() {
    let evil = getNSStringWithUnpairedSurrogate();
    print("\(evil)")
  }
  
}

#if !FOUNDATION_XCTEST
var NSStringTests = TestSuite("TestNSString")
NSStringTests.test("test_equalOverflow") { TestNSString().test_equalOverflow() }
NSStringTests.test("test_smallString_BOM") {
  TestNSString().test_smallString_BOM()
}
NSStringTests.test("test_unpairedSurrogates") {
  TestNSString().test_unpairedSurrogates()
}
runAllTests()
#endif
