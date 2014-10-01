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

import StdlibUnittest

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
    return r as TestManagedBuffer
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
    }
  }
  expectEqual(0, LifetimeTracked.instances)
}

runAllTests()
