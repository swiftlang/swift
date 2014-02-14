//===--- ArrayBuffer.swift - Dynamically-allocated storage for Swift Array ---===//
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
//  This is the class that implements the storage and object management for
//  Swift Array.
//
//===----------------------------------------------------------------------===//

// A HeapBuffer whose storage contains zero or more consecutive Elements
class ArrayBufferStorage<T> 
  : HeapBufferStorage<CountAndCapacity, T> {

  destructor() {
    let p = HeapBuffer(self).elementStorage
    for i in 0...HeapBuffer(self).value.count {
      (p + i).destroy()
    }
  }
}

func arrayBuffer<T>(_:T.metatype, count: Int, capacity: Int) -> HeapBuffer<CountAndCapacity, T> {
  return HeapBuffer(
    ArrayBufferStorage<T>,
    CountAndCapacity(count: count, capacity: capacity),
    capacity
  )
}
