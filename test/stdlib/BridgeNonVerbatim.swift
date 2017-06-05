//===--- BridgeNonVerbatim.swift - Array bridging implementation test -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  When a ContiguousArray<T> is bridged to Objective-C, and T isn't
//  "bridged verbatim," Cocoa operations like objectAtIndex may have
//  to conjure up an object to return, and this object is expected to
//  outlive the array.  
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test
//
// REQUIRES: objc_interop

import Swift
import SwiftShims
import Foundation
import StdlibUnittest
import StdlibUnittestFoundationExtras

struct X : _ObjectiveCBridgeable {
  init(_ value: Int) {
    self.value = value
  }

  func _bridgeToObjectiveC() -> LifetimeTracked {
    return LifetimeTracked(value)
  }

  static func _forceBridgeFromObjectiveC(
    _ x: LifetimeTracked,
    result: inout X?
  ) {
    result = X(x.value)
  }

  static func _conditionallyBridgeFromObjectiveC(
    _ x: LifetimeTracked,
    result: inout X?
  ) -> Bool {
    result = X(x.value)
    return true
  }

  static func _unconditionallyBridgeFromObjectiveC(_ source: LifetimeTracked?)
      -> X {
    var result: X?
    _forceBridgeFromObjectiveC(source!, result: &result)
    return result!
  }

  var value: Int
}

let BridgeNonVerbatimTests = TestSuite("BrideNonVerbatim")

BridgeNonVerbatimTests.test("testing") {
    func testScope() {
      let a = [X(1), X(2), X(3)]
      let nsx: NSArray = a._bridgeToObjectiveC()

      // construction of these tracked objects is lazy
      expectEqual(LifetimeTracked.instances, 0)

      // We can get a single element out
      let one = nsx.object(at: 0) as! LifetimeTracked
      expectEqual(one.value, 1)

      // We can get the element again, but it may not have the same identity
      let anotherOne = nsx.object(at: 0) as! LifetimeTracked
      expectEqualReference(one, anotherOne)

      // Because the elements come back at +0, we really don't want to
      // treat them as objects, or we'll get double deletion
      var objects: [Int] = [0, 0]

      objects.withUnsafeMutableBufferPointer {
        // FIXME: Can't elide signature and use $0 here <rdar://problem/17770732> 
        (buf: inout UnsafeMutableBufferPointer<Int>) -> () in
        nsx.available_getObjects(
          AutoreleasingUnsafeMutablePointer(buf.baseAddress!),
          range: NSRange(location: 1, length: 2))
        return
      }

      var x = objects[0]
      // getObjects yields them at +0:
      expectTrue(_isUnique_native(&x))
    }

    autoreleasepool() {
      testScope()
    }

    // leaks?
    expectEqual(LifetimeTracked.instances, 0)
}

runAllTests()
