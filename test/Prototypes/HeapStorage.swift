//===--- HeapStorage.swift - Replacement for HeapBuffer -------------------===//
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
//  
//
//===----------------------------------------------------------------------===//
// RUN: %target-build-swift -parse-stdlib -Xfrontend -disable-access-control %s -o %t.out
// RUN: %target-run %t.out
// XFAIL: interpret

import Swift
import Darwin
import SwiftShims
import StdlibUnittest

typealias _HeapObject = SwiftShims.HeapObject

// This is already in the standard library
func _roundUpToAlignment(offset: Int, alignment: Int) -> Int {
  _sanityCheck(offset >= 0)
  _sanityCheck(alignment > 0)
  // Note, given that offset is >= 0, and alignment > 0, we don't
  // need to underflow check the -1, as it can never underflow.
  let x = (offset + alignment &- 1)
  // Note, as alignment is a power of 2, we'll use masking to efficiently
  // get the aligned value
  return x & ~(alignment &- 1)
}

@asmname("swift_bufferAllocate")
func _swift_bufferAllocate(
  bufferType: AnyClass, size: Int, alignMask: Int) -> AnyObject

/// A class containing an ivar "value" of type Value, and
/// containing storage for an array of Element whose size is
/// determined at create time.
///
/// The analogous C++-ish class template would be::
///
///   template <class Value, class Element>
///   struct HeapBuffer2 {
///     Value value;
///     Element baseAddress[];        // length determined at creation time
///
///     HeapBuffer2() = delete
///     static shared_ptr<HeapBuffer2> create(Value init, int capacity);
///   }
///
/// Note that the Element array is RAW MEMORY.  You are expected to
/// construct and---if necessary---destroy Elements there yourself,
/// either in a derived class, or it can be in some manager object
/// that owns the HeapBuffer2.
public class HeapStorage<Value, Element> {
  public final class func create(
    minimumCapacity: Int, initialize: (HeapStorage)->Value
  ) -> HeapStorage {
    _precondition(
      minimumCapacity >= 0,
      "HeapStorage must have non-negative capacity")

    let totalSize = HeapStorage._elementOffset
      +  minimumCapacity * strideof(Element.self)

    let alignMask = HeapStorage._alignmentMask

    let result = self._allocate(totalSize, alignMask: alignMask)
    
    result.withUnsafeMutablePointerToValue {
      $0.initialize(initialize(result))
    }
    return result
  }

  internal final class func _allocate(
    totalSize: Int, alignMask: Int
  ) -> HeapStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.bridgeToRawPointer(
        _swift_bufferAllocate(self, totalSize, alignMask)
      ))
  }
  
  internal init() {
    fatalError("Only initialize these by calling create")
  }
  
  /// Destroy the stored Value
  deinit {
    // FIXME: doing the work in a helper is a workaround for
    // <rdar://problem/18158010>
    _deinit()
  }

  // FIXME: separating this from the real deinit is a workaround for
  // <rdar://problem/18158010>
  /// The guts of deinit(); do not call
  internal final func _deinit() {
    withUnsafeMutablePointerToValue { $0.destroy() }
  }

  /// The required alignment for allocations of this type, minus 1
  internal final class var _alignmentMask : Int {
    return max(
      alignof(_HeapObject.self),
      max(alignof(Value.self), alignof(Element.self))) &- 1
  }

  /// The actual number of bytes allocated for this object.
  internal final var _allocatedByteCount : Int {
    return Int(bitPattern: malloc_size(unsafeAddressOf(self)))
  }
  
  /// The actual number of elements allocated for this object.  May be
  /// nontrivial to compute; it may be a good idea to store this
  /// information in the value area upon creation.
  internal final var _allocatedElementCount : Int {
    return (
      _allocatedByteCount &- HeapStorage._elementOffset &+ sizeof(Element) &- 1
    ) &/ sizeof(Element)
  }

  public final var value: Value {
    get {
      return withUnsafeMutablePointerToValue {
        return $0.memory
      }
    }
    set {
      withUnsafeMutablePointerToValue {
        $0.memory = newValue
      }
    }
  }
  
  // Call body with an UnsafeMutablePointer to the stored value
  public final func withUnsafeMutablePointerToValue<R>(
    body: (UnsafeMutablePointer<Value>)->R
  ) -> R {
    return withUnsafeMutablePointers { (v, e) in return body(v) }
  }
  
  // Call body with an UnsafeMutablePointer to the stored value
  public final func withUnsafeMutablePointerToElements<R>(
    body: (UnsafeMutablePointer<Element>)->R
  ) -> R {
    return withUnsafeMutablePointers { return body($0.1) }
  }

  public final func withUnsafeMutablePointers<R>(
    body: (_: UnsafeMutablePointer<Value>, _: UnsafeMutablePointer<Element>)->R
  ) -> R {
    let result = body(
      UnsafeMutablePointer(_address + HeapStorage._valueOffset),
      UnsafeMutablePointer(_address + HeapStorage._elementOffset)
    )
    _fixLifetime(self)
    return result
  }
  
  /// The address of this instance in a convenient pointer-to-bytes form
  internal final var _address : UnsafePointer<UInt8> {
    return UnsafePointer(unsafeAddressOf(self))
  }

  internal final class var _valueOffset : Int {
    return _roundUpToAlignment(sizeof(_HeapObject.self), alignof(Value.self))
  }

  internal final class var _elementOffset : Int {
    return _roundUpToAlignment(
      _valueOffset + sizeof(Value.self), alignof(Element.self))
  }
  
  /// A hack that gives the deallocator information about our
  /// allocated size.  Probably completely unused per
  /// <rdar://problem/18156440>
  internal final func __getInstanceSizeAndAlignMask() -> (Int,Int) {
    return (_allocatedByteCount, HeapStorage._alignmentMask)
  }
}

//===----------------------------------------------------------------------===//
//===--- Testing code -----------------------------------------------------===//
//===----------------------------------------------------------------------===//

struct CountAndCapacity {
  var count: LifetimeTracked
  let capacity: Int = 0
}

final class TestHeapStorage<T> : HeapStorage<CountAndCapacity,T> {
  class func create(capacity: Int) -> TestHeapStorage {
    let r = self.create(capacity) {
      CountAndCapacity(
        count: LifetimeTracked(0), capacity: $0._allocatedElementCount)
    }
    return r as TestHeapStorage
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
  
  func teardown() {
    let count = self.count
    
    withUnsafeMutablePointerToElements {
      (x: UnsafeMutablePointer<T>)->() in
      for i in stride(from: 0, to: count, by: 2) {
        println("destroying element \(i)")
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

var tests = TestSuite("HeapStorage")

tests.test("basic") {
  expectEqual(0, LifetimeTracked.instances)
  if true {
    let s = TestHeapStorage<LifetimeTracked>.create(0)
    expectEqual(1, LifetimeTracked.instances)
  }
  
  expectEqual(0, LifetimeTracked.instances)
  if true {
    let s = TestHeapStorage<LifetimeTracked>.create(10)
    expectEqual(0, s.count)
    expectEqual(10, s.capacity)
    
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
