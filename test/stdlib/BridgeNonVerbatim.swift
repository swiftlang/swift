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
// RUN: %target-run-stdlib-swift %s | %FileCheck %s
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

// CHECK: testing...
print("testing...")

func testScope() {
  let a = [X(1), X(2), X(3)]
  let nsx: NSArray = a._bridgeToObjectiveC()

  // construction of these tracked objects is lazy
  // CHECK-NEXT: trackedCount = 0 .
  print("trackedCount = \(LifetimeTracked.instances) .")

  // We can get a single element out
  // CHECK-NEXT: nsx[0]: 1 .
  let one = nsx.object(at: 0) as! LifetimeTracked
  print("nsx[0]: \(one.value) .")

  // We can get the element again, but it may not have the same identity
  // CHECK-NEXT: object identity matches?
  let anotherOne = nsx.object(at: 0) as! LifetimeTracked
  print("object identity matches? \(one === anotherOne)")

  // Because the elements come back at +0, we really don't want to
  // treat them as objects, or we'll get double deletion
  var objects: [Int] = [0, 0]

  objects.withUnsafeMutableBufferPointer {
    // FIXME: Can't elide signature and use $0 here <rdar://problem/17770732> 
    (buf: inout UnsafeMutableBufferPointer<Int>) -> () in
    nsx.available_getObjects(
      AutoreleasingUnsafeMutablePointer(buf.baseAddress), // Michael NOTE: unsafe buffer pointer ! change
      range: NSRange(location: 1, length: 2))
    return
  }

  // CHECK-NEXT: getObjects yields them at +0: true
  var x = objects[0]
  print("getObjects yields them at +0: "
    + "\(_isUnique_native(&x))")
}

autoreleasepool() {
  testScope()
}

// CHECK-NEXT: leaks = 0 .
print("leaks = \(LifetimeTracked.instances) .")

