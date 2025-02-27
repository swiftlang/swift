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
  internal var count: Int

  internal init(_ count: Int) { self.count = count }
}

// NOTE: older runtimes called this class _BridgingBufferStorage.
// The two must coexist without a conflicting ObjC class name, so it
// was renamed. The old name must not be used in the new runtime.
internal final class __BridgingBufferStorage
  : ManagedBuffer<_BridgingBufferHeader, AnyObject> {
}

internal typealias _BridgingBuffer
  = ManagedBufferPointer<_BridgingBufferHeader, AnyObject>

@available(OpenBSD, unavailable, message: "malloc_size is unavailable.")
extension ManagedBufferPointer
where Header == _BridgingBufferHeader, Element == AnyObject {
  internal init(_ count: Int) {
    self.init(
      _uncheckedBufferClass: __BridgingBufferStorage.self,
      minimumCapacity: count)
    unsafe self.withUnsafeMutablePointerToHeader {
      unsafe $0.initialize(to: Header(count))
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
      return unsafe withUnsafeMutablePointerToElements { unsafe $0[i] }
    }
  }

  internal var baseAddress: UnsafeMutablePointer<Element> {
    @inline(__always)
    get {
      return unsafe withUnsafeMutablePointerToElements { unsafe $0 }
    }
  }

  internal var storage: AnyObject? {
    @inline(__always)
    get {
      return buffer
    }
  }
}
