//===----------------------------------------------------------------------===//
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

import SwiftShims

@usableFromInline // FIXME(sil-serialize-all)
internal typealias _HeapObject = SwiftShims.HeapObject

@usableFromInline
internal protocol _HeapBufferHeader_ {
  associatedtype Value
  init(_ value: Value)
  var value: Value { get set }
}

@_fixed_layout // FIXME(sil-serialize-all)
@usableFromInline
internal struct _HeapBufferHeader<T> : _HeapBufferHeader_ {
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Value = T
  @inlinable // FIXME(sil-serialize-all)
  internal init(_ value: T) { self.value = value }
  @usableFromInline // FIXME(sil-serialize-all)
  internal var value: T
}

@usableFromInline
internal typealias _HeapBuffer<Value,Element>
  = ManagedBufferPointer<_HeapBufferHeader<Value>, Element>

@usableFromInline
internal typealias _HeapBufferStorage<Value,Element>
  = ManagedBuffer<_HeapBufferHeader<Value>, Element>

extension ManagedBufferPointer where Header : _HeapBufferHeader_ {
  @usableFromInline // FIXME(sil-serialize-all)
  internal typealias Value = Header.Value

  @inlinable // FIXME(sil-serialize-all)
  internal init(
    _ storageClass: AnyClass,
    _ initializer: Value, _ capacity: Int
  ) {
    self.init(
      _uncheckedBufferClass: storageClass,
      minimumCapacity: capacity)
    self.withUnsafeMutablePointerToHeader {
      $0.initialize(to: Header(initializer))
    }
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal var value: Value {
    @inline(__always)
    get {
      return header.value
    }
    @inline(__always)
    set {
      return header.value = newValue
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal subscript(i: Int) -> Element {
    @inline(__always)
    get {
      return withUnsafeMutablePointerToElements { $0[i] }
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal var baseAddress: UnsafeMutablePointer<Element> {
    @inline(__always)
    get {
      return withUnsafeMutablePointerToElements { $0 }
    }
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal var storage: AnyObject? {
    @inline(__always)
    get {
      return buffer
    }
  }
}
