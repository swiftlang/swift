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

// The class that implements the storage for ArrayBuffer<T>
class ArrayStorage<T> : HeapBufferStorage<CountAndCapacity,T> {
  destructor() {
    let b = HeapBuffer(self)
    for i in 0...b.value.count {
      (b.elementStorage + i).destroy()
    }
  }
}

// A resizeable buffer of Ts
struct ArrayBuffer<T> {
  typealias Storage = ArrayStorage<T>

  init() {
    buffer = HeapBuffer()
  }
  
  init(s: Storage) {
    buffer = HeapBuffer(s)
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
  
  var buffer: HeapBuffer<CountAndCapacity, T>
}

// CHECK: all done.
println("all done.")
