//===--- ManagedBuffer.swift ----------------------------------------------===//
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
// RUN: %target-run-simple-swift

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import StdlibUnittest
import Foundation

struct CountAndCapacity {
  var count: LifetimeTracked
  let capacity: Int = 0
}

// An example of ManagedBuffer, very similar to what Array will use.
// However, only half of the element storage is actually used to store
// elements, interleaved with garbage, as a simple way of catching
// potential bugs.
final class TestManagedBuffer<T> : ManagedBuffer<CountAndCapacity,T> {
  class func create(capacity: Int) -> TestManagedBuffer {
    let r = super.create(capacity) {
      CountAndCapacity(
        count: LifetimeTracked(0), capacity: $0.allocatedElementCount)
    }
    return r as! TestManagedBuffer
  }

  var count: Int {
    get {
      return value.count.value
    }
    set {
      value.count = LifetimeTracked(newValue)
    }
  }
  
  var capacity: Int {
    return value.capacity
  }
  
  deinit {
    teardown()
  }

  // This doesn't seem to compile properly when embedded directly in
  // deinit.
  func teardown() {
    let count = self.count
    
    withUnsafeMutablePointerToElements {
      (x: UnsafeMutablePointer<T>)->() in
      for i in stride(from: 0, to: count, by: 2) {
        (x + i).destroy()
      }
    }
  }
  
  func append(x: T) {
    let count = self.count
    precondition(count + 2 <= capacity)
    
    withUnsafeMutablePointerToElements {
      (p: UnsafeMutablePointer<T>)->() in
      (p + count).initialize(x)
    }
    self.count = count + 2
  }
}

class MyBuffer<T> {
  typealias Manager = ManagedBufferPointer<CountAndCapacity, T>
  deinit {
    Manager(unsafeBufferObject: self).withUnsafeMutablePointers {
      (pointerToValue, pointerToElements)->Void in
      pointerToElements.destroy(self.count)
      pointerToValue.destroy()
    }
  }

  var count: Int {
    return Manager(unsafeBufferObject: self).value.count.value
  }
  var capacity: Int {
    return Manager(unsafeBufferObject: self).value.capacity
  }
}

var tests = TestSuite("ManagedBuffer")

tests.test("basic") {
  expectEqual(0, LifetimeTracked.instances)
  if true {
    let s = TestManagedBuffer<LifetimeTracked>.create(0)
    expectEqual(1, LifetimeTracked.instances)
  }
  
  expectEqual(0, LifetimeTracked.instances)
  if true {
    let s = TestManagedBuffer<LifetimeTracked>.create(10)
    expectEqual(0, s.count)
    expectLE(10, s.capacity)
    expectGE(12, s.capacity)  // allow some over-allocation but not too much
    
    expectEqual(1, LifetimeTracked.instances)
    for i in 1..<6 {
      s.append(LifetimeTracked(i))
      expectEqual(i + 1, LifetimeTracked.instances)
      expectEqual(i * 2, s.count)
      expectEqual(
        s.count,
        s.withUnsafeMutablePointerToValue { $0.memory.count.value }
      )
      expectEqual(
        s.capacity,
        s.withUnsafeMutablePointerToValue { $0.memory.capacity }
      )
      expectEqual(
        LifetimeTracked(i),
        s.withUnsafeMutablePointerToElements { $0[(i - 1) * 2] }
      )
    }
  }
  expectEqual(0, LifetimeTracked.instances)
}

tests.test("ManagedBufferPointer/SizeValidation/TestmanagedBuffer") {
  let x = ManagedBufferPointer<CountAndCapacity, LifetimeTracked>(
    bufferClass: TestManagedBuffer<LifetimeTracked>.self,
    minimumCapacity: 10
  ) {
    buffer, getRealCapacity in 
    CountAndCapacity(
      count: LifetimeTracked(0), capacity: getRealCapacity(buffer))
  }
}

tests.test("ManagedBufferPointer/SizeValidation/MyBuffer") {
  let x = ManagedBufferPointer<CountAndCapacity, LifetimeTracked>(
    bufferClass:  MyBuffer<LifetimeTracked>.self,
    minimumCapacity: 0
  ) { _, _ in CountAndCapacity(count: LifetimeTracked(0), capacity: 99) }
}

tests.test("ManagedBufferPointer") {
  typealias Manager = ManagedBufferPointer<CountAndCapacity, LifetimeTracked>

  if true {
    var mgr = Manager(
      bufferClass: TestManagedBuffer<LifetimeTracked>.self,
      minimumCapacity: 10
    ) {
      buffer, getRealCapacity in 
      CountAndCapacity(
        count: LifetimeTracked(0), capacity: getRealCapacity(buffer))
    }
    expectTrue(mgr.holdsUniqueReference())

    let buf = mgr.buffer as? TestManagedBuffer<LifetimeTracked>
    expectTrue(buf != nil)
    expectFalse(mgr.holdsUniqueReference())
    
    let s = buf!
    expectEqual(0, s.count)
    expectLE(10, s.capacity)
    expectGE(12, s.capacity)  // allow some over-allocation but not too much
    
    expectEqual(s.count, mgr.value.count.value)
    expectEqual(s.capacity, mgr.value.capacity)

    expectEqual(
      mgr.withUnsafeMutablePointerToValue { $0 },
      s.withUnsafeMutablePointerToValue { $0 })
    
    expectEqual(
      mgr.withUnsafeMutablePointerToElements { $0 },
      s.withUnsafeMutablePointerToElements { $0 })
    
    for i in 1..<6 {
      s.append(LifetimeTracked(i))
      expectEqual(i * 2, s.count)
      expectEqual(s.count, mgr.value.count.value)
    }
    
    mgr = Manager(
      bufferClass:  MyBuffer<LifetimeTracked>.self,
      minimumCapacity: 0
    ) { _, _ in CountAndCapacity(count: LifetimeTracked(0), capacity: 99) }

    expectTrue(mgr.holdsUniqueReference())
    expectEqual(mgr.value.count.value, 0)
    expectEqual(mgr.value.capacity, 99)

    let s2 = mgr.buffer as! MyBuffer<LifetimeTracked>
    expectFalse(mgr.holdsUniqueReference())
    
    let val = mgr.withUnsafeMutablePointerToValue { $0 }.memory
    expectEqual(val.count.value, 0)
    expectEqual(val.capacity, 99)
  }
  expectEqual(0, LifetimeTracked.instances)
}

tests.test("isUniquelyReferenced") {
  var s = TestManagedBuffer<LifetimeTracked>.create(0)
  expectTrue(isUniquelyReferenced(&s))
  var s2 = s
  expectFalse(isUniquelyReferenced(&s))
  expectFalse(isUniquelyReferenced(&s2))
}

tests.test("isUniquelyReferencedNonObjC") {
  var s = TestManagedBuffer<LifetimeTracked>.create(0)
  expectTrue(isUniquelyReferencedNonObjC(&s))
  var s2 = s
  expectFalse(isUniquelyReferencedNonObjC(&s))
  expectFalse(isUniquelyReferencedNonObjC(&s2))
  var s3 = NSArray()
  expectFalse(isUniquelyReferencedNonObjC(&s3))
}

runAllTests()
