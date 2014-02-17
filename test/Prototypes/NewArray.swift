//===--- NewArray.swift ---------------------------------------------------===//
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

// RUN: %target-run-stdlib-swift | FileCheck %s

import swift

// The class that implements the storage for Array<T>
class ArrayStorage<T> : HeapBufferStorage<CountAndCapacity,T> {
  destructor() {
    val b = HeapBuffer(self)
    for i in 0...b.value.count {
      (b.elementStorage + i).destroy()
    }
  }
}

// A resizeable buffer of Ts
struct Array<T> {
  typealias Storage = ArrayStorage<T>
  typealias Buffer = HeapBuffer<CountAndCapacity,T>

  init() {
    buffer = Buffer()
  }
  
  init(s: Storage) {
    buffer = Buffer(s)
  }

  var count: Int {
    return buffer ? buffer.value.count : 0
  }
  
  var capacity: Int {
    return buffer ? buffer.value.capacity : 0
  }

  subscript(i: Int) -> T {
  get:
    assert(i >= 0 && i < count, "Index out-of-range")
    return buffer[i]
  set(newValue):
    assert(i >= 0 && i < count, "Index out-of-range")
    buffer[i] = newValue
  }

  // Return a uniquely-referenced HeapBuffer capable of storing at
  // least newCount elements.
  // 
  // NOTE: If the resulting Buffer this Array's buffer, it contains
  // the same initialized elements that it did before the call.
  // Otherwise, no new elements will have been initialized.
  // Regardless, new elements need to be initialized
  func _demandUniqueCount(newCount: Int) -> Buffer {
    assert(newCount >= count, "Don't call this to shrink the buffer")
    if _fastPath(buffer.isUniquelyReferenced() && capacity >= newCount) {
      buffer.value.count = newCount
      return buffer
    }
    return Array._newBuffer(
      newCount, newCount > capacity ? max(newCount, capacity * 2) : capacity
    )
  }

  mutating func append(x: T) {
    self += CollectionOfOne(x)
  }

  //===--- private ----------------------------------------------------------===//
  static func _newBuffer(count: Int, capacity: Int) -> Buffer {
    return Buffer(Storage, CountAndCapacity(count, capacity), capacity)
  }

  var buffer: Buffer
}

@assignment
func += <
  T,
  Other: Collection
     where Other.GeneratorType.Element == T, Other.IndexType : RandomAccessIndex
>(
  inout lhs: Array<T>, rhs: Other
) {
  val oldCount = lhs.count
  val oldBuffer = lhs.buffer
  val newCount = oldCount + rhs.endIndex()~>distanceTo(rhs.startIndex())
  val newBuffer = lhs._demandUniqueCount(newCount)
  
  if _slowPath(newBuffer != oldBuffer) {
    var src = oldBuffer.elementStorage
    val srcEnd = src + oldCount
    var dst = newBuffer.elementStorage

    // We have the only reference to the buffer, so we can move
    // elements instead of copying them
    while src != srcEnd {
      dst++.initialize(src++.move())
    }

    // Don't destroy the elements; they've been moved
    oldBuffer.value.count = 0
    lhs.buffer = newBuffer // val the old buffer go
  }

  // At this point we still have only oldCount initialized elements;
  // fill the buffer up to newCount
  assert(lhs.count == newCount)
  var p = newBuffer.elementStorage + oldCount
  for x in rhs {
    p++.initialize(x)
  }
}

// CHECK: all done.
println("all done.")
