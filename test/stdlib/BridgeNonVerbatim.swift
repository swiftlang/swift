//===--- BridgeNonVerbatim.swift - Array bridging implementation test -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  When a ContiguousArray<T> is bridged to Objective-C, and T isn't
//  "bridged verbatim," Cocoa operations like objectAtIndex may have
//  to conjure up an object to return, and this object is expected to
//  outlive the array.  
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift %s | FileCheck %s

import SwiftShims
import ObjectiveC

//===--- class Tracked ----------------------------------------------------===//
// Instead of testing with Int elements, we use this wrapper class
// that can help us track allocations and find issues with object
// lifetime inside Array implementations.
var trackedCount = 0
var nextTrackedSerialNumber = 0

class Tracked : ForwardIndexType, Printable {
  init(_ value: Int) {
    ++trackedCount
    serialNumber = ++nextTrackedSerialNumber
    self.value = value
  }
  
  deinit {
    assert(serialNumber > 0, "double destruction!")
    --trackedCount
    serialNumber = -serialNumber
  }

  var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return value.description
  }

  func successor() -> Tracked {
    return Tracked(self.value.successor())
  }

  var value: Int
  var serialNumber: Int
}

func == (x: Tracked, y: Tracked) -> Bool {
  return x.value == y.value
}

struct X : _BridgedToObjectiveCType {
  init(_ value: Int) {
    self.value = value
  }

  static func _getObjectiveCType() -> Any.Type {
    return Tracked.self
  }

  func _bridgeToObjectiveC() -> Tracked {
    return Tracked(value)
  }

  static func _bridgeFromObjectiveC(x: Tracked) -> X {
    return X(x.value)
  }

  var value: Int
}

// CHECK: testing...
println("testing...")

func testScope() {
  let a = [X(1), X(2), X(3)]
  let nsx = a._asCocoaArray()

  // construction of these tracked objects is lazy
  // CHECK-NEXT: trackedCount = 0 .
  println("trackedCount = \(trackedCount) .")

  // We can get a single element out
  // CHECK-NEXT: nsx[0]: 1 .
  var one = nsx.objectAtIndex(0) as Tracked
  println("nsx[0]: \(one.value) .")

  // We can get the element again, but it may not have the same identity
  // CHECK-NEXT: object identity matches?
  var anotherOne = nsx.objectAtIndex(0) as Tracked
  println("object identity matches? \(one === anotherOne)")

  // Because the elements come back at +0, we really don't want to
  // treat them as objects, or we'll get double deletion
  let objects: [Word] = [0, 0]

  objects.withUnsafePointerToElements {
    nsx.getObjects(
      UnsafePointer($0), // pointer cast
      range: _SwiftNSRange(location: 1, length: 2))
  }

  // CHECK-NEXT: getObjects yields them at +0: true
  var x = objects[0]
  println("getObjects yields them at +0: \(_isUniquelyReferenced(&x))")
}

autoreleasepool() {
  testScope()
}

// CHECK-NEXT: leaks = 0 .
println("leaks = \(trackedCount) .")

