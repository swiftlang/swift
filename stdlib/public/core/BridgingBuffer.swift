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

internal struct _BridgingBufferHeader {
  internal init(_ count: Int) { self.count = count }
  internal var count: Int
}

internal final class _BridgingBufferStorage
  : ManagedBuffer<_BridgingBufferHeader, AnyObject> {
}

internal typealias _BridgingBuffer
  = ManagedBufferPointer<_BridgingBufferHeader, AnyObject>

extension ManagedBufferPointer
where Header == _BridgingBufferHeader, Element == AnyObject {
  internal init(_ count: Int) {
    self.init(
      _uncheckedBufferClass: _BridgingBufferStorage.self,
      minimumCapacity: count)
    self.withUnsafeMutablePointerToHeader {
      $0.initialize(to: Header(count))
    }
  }

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

  internal subscript(i: Int) -> Element {
    @inline(__always)
    get {
      return withUnsafeMutablePointerToElements { $0[i] }
    }
  }

  internal var baseAddress: UnsafeMutablePointer<Element> {
    @inline(__always)
    get {
      return withUnsafeMutablePointerToElements { $0 }
    }
  }

  internal var storage: AnyObject? {
    @inline(__always)
    get {
      return buffer
    }
  }
}
