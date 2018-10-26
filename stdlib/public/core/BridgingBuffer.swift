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

@_fixed_layout
@usableFromInline
internal struct _BridgingBufferHeader {
  @inlinable
  internal init(_ count: Int) { self.count = count }
  @usableFromInline
  internal var count: Int
}

@usableFromInline
@_fixed_layout
internal final class _BridgingBufferStorage
  : ManagedBuffer<_BridgingBufferHeader, AnyObject> {
}

@usableFromInline
internal typealias _BridgingBuffer
  = ManagedBufferPointer<_BridgingBufferHeader, AnyObject>

extension ManagedBufferPointer
where Header == _BridgingBufferHeader, Element == AnyObject {
  @inlinable
  internal init(_ count: Int) {
    self.init(
      _uncheckedBufferClass: _BridgingBufferStorage.self,
      minimumCapacity: count)
    self.withUnsafeMutablePointerToHeader {
      $0.initialize(to: Header(count))
    }
  }

  @inlinable
  internal var count: Int {
    @inline(__always)
    get {
      return header.count
    }
    @inline(__always)
    set {
      return header.count = newValue
    }
  }

  @inlinable
  internal subscript(i: Int) -> Element {
    @inline(__always)
    get {
      return withUnsafeMutablePointerToElements { $0[i] }
    }
  }

  @inlinable
  internal var baseAddress: UnsafeMutablePointer<Element> {
    @inline(__always)
    get {
      return withUnsafeMutablePointerToElements { $0 }
    }
  }

  @inlinable
  internal var storage: AnyObject? {
    @inline(__always)
    get {
      return buffer
    }
  }
}
