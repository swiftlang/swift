//===--- ManagedBuffer.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import StdlibUnittest
import Foundation

// Check that `NonObjectiveCBase` can be subclassed and the subclass can be
// created.
public class SubclassOfNonObjectiveCBase : NonObjectiveCBase {
  public override init() {}
}
func createSubclassOfNonObjectiveCBase() {
  _ = SubclassOfNonObjectiveCBase()
}

// Check that the generic parameters are called 'Value' and 'Element'.
protocol TestProtocol1 {}

extension ManagedProtoBuffer
  where Value : TestProtocol1, Element : TestProtocol1 {

  var _valueAndElementAreTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

extension ManagedBuffer
  where Value : TestProtocol1, Element : TestProtocol1 {

  var _valueAndElementAreTestProtocol1_: Bool {
    fatalError("not implemented")
  }
}

extension ManagedBufferPointer
  where Value : TestProtocol1, Element : TestProtocol1 {

  var _valueAndElementAreTestProtocol1: Bool {
    fatalError("not implemented")
  }
}

struct LengthAndCapacity {
  var length: LifetimeTracked
  let capacity: Int
}

// An example of ManagedBuffer, very similar to what Array will use.
// However, only half of the element storage is actually used to store
// elements, interleaved with garbage, as a simple way of catching
// potential bugs.
final class TestManagedBuffer<T> : ManagedBuffer<LengthAndCapacity,T> {
  class func create(capacity: Int) -> TestManagedBuffer {
    let r = super.create(capacity) {
      LengthAndCapacity(
        length: LifetimeTracked(0), capacity: $0.capacity)
    }
    return r as! TestManagedBuffer
  }

  var length: Int {
    get {
      return value.length.value
    }
    set {
      value.length = LifetimeTracked(newValue)
    }
  }
  
  var myCapacity: Int {
    return value.capacity
  }
  
  deinit {
    teardown()
  }

  // This doesn't seem to compile properly when embedded directly in
  // deinit.
  func teardown() {
    let length = self.length
    
    withUnsafeMutablePointerToElements {
      (x: UnsafeMutablePointer<T>) -> () in
      for i in 0.strideTo(length, by: 2) {
        (x + i).deinitializePointee()
      }
    }
  }
  
  func append(x: T) {
    let length = self.length
    require(length + 2 <= myCapacity)
    
    withUnsafeMutablePointerToElements {
      (p: UnsafeMutablePointer<T>) -> () in
      (p + length).initializePointee(x)
    }
    self.length = length + 2
  }
}

class MyBuffer<T> {
  typealias Manager = ManagedBufferPointer<LengthAndCapacity, T>
  deinit {
    Manager(unsafeBufferObject: self).withUnsafeMutablePointers {
      (pointerToValue, pointerToElements) -> Void in
      pointerToElements.deinitializePointee(count: self.length)
      pointerToValue.deinitializePointee()
    }
  }

  var length: Int {
    return Manager(unsafeBufferObject: self).value.length.value
  }
  var capacity: Int {
    return Manager(unsafeBufferObject: self).value.capacity
  }
}

var tests = TestSuite("ManagedBuffer")

tests.test("basic") {
  if true {
    let s = TestManagedBuffer<LifetimeTracked>.create(0)
    expectEqual(1, LifetimeTracked.instances)
  }
  
  expectEqual(0, LifetimeTracked.instances)
  if true {
    let s = TestManagedBuffer<LifetimeTracked>.create(10)
    expectEqual(0, s.length)
    expectLE(10, s.myCapacity)
    expectGE(12, s.myCapacity)  // allow some over-allocation but not too much
    
    expectEqual(1, LifetimeTracked.instances)
    for i in 1..<6 {
      s.append(LifetimeTracked(i))
      expectEqual(i + 1, LifetimeTracked.instances)
      expectEqual(i * 2, s.length)
      expectEqual(
        s.length,
        s.withUnsafeMutablePointerToValue { $0.pointee.length.value }
      )
      expectEqual(
        s.myCapacity,
        s.withUnsafeMutablePointerToValue { $0.pointee.capacity }
      )
      expectEqual(
        LifetimeTracked(i),
        s.withUnsafeMutablePointerToElements { $0[(i - 1) * 2] }
      )
    }
  }
}

tests.test("ManagedBufferPointer/SizeValidation/TestmanagedBuffer") {
  let x = ManagedBufferPointer<LengthAndCapacity, LifetimeTracked>(
    bufferClass: TestManagedBuffer<LifetimeTracked>.self,
    minimumCapacity: 10
  ) {
    buffer, getRealCapacity in 
    LengthAndCapacity(
      length: LifetimeTracked(0), capacity: getRealCapacity(buffer))
  }
}

tests.test("ManagedBufferPointer/SizeValidation/MyBuffer") {
  let x = ManagedBufferPointer<LengthAndCapacity, LifetimeTracked>(
    bufferClass:  MyBuffer<LifetimeTracked>.self,
    minimumCapacity: 0
  ) { _, _ in LengthAndCapacity(length: LifetimeTracked(0), capacity: 99) }
}

tests.test("ManagedBufferPointer") {
  typealias Manager = ManagedBufferPointer<LengthAndCapacity, LifetimeTracked>

  if true {
    var mgr = Manager(
      bufferClass: TestManagedBuffer<LifetimeTracked>.self,
      minimumCapacity: 10
    ) {
      buffer, getRealCapacity in 
      LengthAndCapacity(
        length: LifetimeTracked(0), capacity: getRealCapacity(buffer))
    }
    expectTrue(mgr.holdsUniqueReference())

    let buf = mgr.buffer as? TestManagedBuffer<LifetimeTracked>
    expectTrue(buf != nil)
    expectFalse(mgr.holdsUniqueReference())
    
    let s = buf!
    expectEqual(0, s.length)
    expectLE(10, s.capacity)
    expectGE(12, s.capacity)  // allow some over-allocation but not too much
    
    expectEqual(s.length, mgr.value.length.value)
    expectEqual(s.capacity, mgr.value.capacity)

    expectEqual(
      mgr.withUnsafeMutablePointerToValue { $0 },
      s.withUnsafeMutablePointerToValue { $0 })
    
    expectEqual(
      mgr.withUnsafeMutablePointerToElements { $0 },
      s.withUnsafeMutablePointerToElements { $0 })
    
    for i in 1..<6 {
      s.append(LifetimeTracked(i))
      expectEqual(i * 2, s.length)
      expectEqual(s.length, mgr.value.length.value)
    }
    
    mgr = Manager(
      bufferClass:  MyBuffer<LifetimeTracked>.self,
      minimumCapacity: 0
    ) { _, _ in LengthAndCapacity(length: LifetimeTracked(0), capacity: 99) }

    expectTrue(mgr.holdsUniqueReference())
    expectEqual(mgr.value.length.value, 0)
    expectEqual(mgr.value.capacity, 99)

    let s2 = mgr.buffer as! MyBuffer<LifetimeTracked>
    expectFalse(mgr.holdsUniqueReference())
    
    let val = mgr.withUnsafeMutablePointerToValue { $0 }.pointee
    expectEqual(val.length.value, 0)
    expectEqual(val.capacity, 99)
  }
}

tests.test("isUniquelyReferenced") {
  var s = TestManagedBuffer<LifetimeTracked>.create(0)
  expectTrue(isUniquelyReferenced(&s))
  var s2 = s
  expectFalse(isUniquelyReferenced(&s))
  expectFalse(isUniquelyReferenced(&s2))
  _fixLifetime(s)
  _fixLifetime(s2)
}

tests.test("isUniquelyReferencedNonObjC") {
  var s = TestManagedBuffer<LifetimeTracked>.create(0)
  expectTrue(isUniquelyReferencedNonObjC(&s))
  var s2 = s
  expectFalse(isUniquelyReferencedNonObjC(&s))
  expectFalse(isUniquelyReferencedNonObjC(&s2))
  var s3 = NSArray()
  expectFalse(isUniquelyReferencedNonObjC(&s3))
  _fixLifetime(s)
  _fixLifetime(s2)
}

runAllTests()
