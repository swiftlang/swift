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
  
}

#if !FOUNDATION_XCTEST
var NSStringTests = TestSuite("TestNSString")
NSStringTests.test("test_equalOverflow") { TestNSString().test_equalOverflow() }
runAllTests()
#endif
