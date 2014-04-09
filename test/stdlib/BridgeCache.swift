//===--- BridgeCache.swift - Array bridging implementation test -----------===//
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
//  When a NativeArray<T> is bridged to Objective-C, and T isn't
//  "bridged verbatim," Cocoa operations like objectAtIndex may have
//  to conjure up an object to return, and this object is expected to
//  outlive the array.  We cache these objects so that their lifetimes
//  are guaranteed.  Here we test the cache.
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift %s | FileCheck %s

import SwiftShims

//===--- class Tracked ----------------------------------------------------===//
// Instead of testing with Int elements, we use this wrapper class
// that can help us track allocations and find issues with object
// lifetime inside Array implementations.
var trackedCount = 0
var nextTrackedSerialNumber = 0

class Tracked : ReplPrintable, ForwardIndex, ObjCClassType {
  init(value: Int) {
    ++trackedCount
    serialNumber = ++nextTrackedSerialNumber
    self.value = value
  }
  
  deinit {
    assert(serialNumber > 0, "double destruction!")
    --trackedCount
    serialNumber = -serialNumber
  }

  func replPrint() {
    assert(serialNumber > 0, "dead Tracked!")
    value.replPrint()
  }

  func succ() -> Tracked {
    return Tracked(self.value.succ())
  }

  var value: Int
  var serialNumber: Int
}

func == (x: Tracked, y: Tracked) -> Bool {
  return x.value == y.value
}

struct X : BridgedToObjectiveC {
  func bridgeToObjectiveC() -> Tracked {
    return Tracked(value)
  }
  var value: Int
}

// CHECK: testing...
println("testing...")

func testScope() {
  let a = [X(1), X(2), X(3)]
  let nsx = a.asCocoaArray()

  // construction of these tracked objects is lazy
  // CHECK-NEXT: trackedCount = 0 .
  println("trackedCount = \(trackedCount) .")

  // We can get a single element out
  // CHECK-NEXT: nsx[0]: 1 .
  var one = (nsx.objectAtIndex(0) as Tracked)!
  println("nsx[0]: \(one.value) .")

  // If we get the element again, it has the same object identity
  // CHECK-NEXT: object identity matches: true
  var anotherOne = (nsx.objectAtIndex(0) as Tracked)!
  println("object identity matches: \(one === anotherOne)")

  // We've only constructed one such object
  // CHECK-NEXT: trackedCount = 1 .
  println("trackedCount = \(trackedCount) .")

  // Because the elements come back at +0, we really don't want to
  // treat them as objects, or we'll get double deletion
  let objects: Word[] = [0, 0]
  
  nsx.getObjects(
    UnsafePointer(objects.buffer.elementStorage), // pointer cast
    _SwiftNSRange(1, 2))

  // CHECK-NEXT: getObjects yields them at +0: true
  var x = objects[0]
  println("getObjects yields them at +0: \(isUniquelyReferenced(&x))")
  // CHECK-NEXT: trackedCount = 3 .
  
  println("trackedCount = \(trackedCount) .")
}
testScope()

// FIXME: should be "leaks = 0" but for <rdar://problem/16562426>
// CHECK-NEXT: leaks = 1 .
println("leaks = \(trackedCount) .")

