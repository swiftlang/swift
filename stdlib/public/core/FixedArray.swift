//===--- FixedArray.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  A helper struct to provide fixed-sized array like functionality
//
//===----------------------------------------------------------------------===//

internal struct _FixedArray2<T> {
  // ABI TODO: The has assumptions about tuple layout in the ABI, namely that
  // they are laid out contiguously and individually addressable (i.e. strided).
  //
  internal var storage: (
    // A 2-wide tuple of type T
    T, T
  )

  var _count: Int8
}

extension _FixedArray2 {
  internal static var capacity: Int {
    @inline(__always) get { return 2 }
  }

  internal var capacity: Int {
    @inline(__always) get { return 2 }
  }

  internal var count: Int {
    @inline(__always) get { return Int(truncatingIfNeeded: _count) }
    @inline(__always) set { _count = Int8(newValue) }
  }
}

extension _FixedArray2: RandomAccessCollection, MutableCollection {
  internal typealias Index = Int

  internal var startIndex: Index {
    return 0
  }

  internal var endIndex: Index {
    return count
  }

  internal subscript(i: Index) -> T {
    @inline(__always)
    get {
      let count = self.count // for exclusive access
      _internalInvariant(i >= 0 && i < count)
      let res: T = withUnsafeBytes(of: storage) {
        (rawPtr : UnsafeRawBufferPointer) -> T in
        let stride = MemoryLayout<T>.stride
        _internalInvariant(rawPtr.count == 2*stride, "layout mismatch?")
        let bufPtr = UnsafeBufferPointer(
          start: rawPtr.baseAddress!.assumingMemoryBound(to: T.self),
          count: count)
        return bufPtr[_unchecked: i]
      }
      return res
    }
    @inline(__always)
    set {
      _internalInvariant(i >= 0 && i < count)
      self.withUnsafeMutableBufferPointer { buffer in
        buffer[_unchecked: i] = newValue
      }
    }
  }

  @inline(__always)
  internal func index(after i: Index) -> Index {
    return i+1
  }

  @inline(__always)
  internal func index(before i: Index) -> Index {
    return i-1
  }
}

extension _FixedArray2 {
  internal mutating func append(_ newElement: T) {
    _internalInvariant(count < capacity)
    _count += 1
    self[count-1] = newElement
  }
}

extension _FixedArray2 where T: ExpressibleByIntegerLiteral {
  @inline(__always)
  internal init(count: Int) {
    _internalInvariant(count >= 0 && count <= _FixedArray2.capacity)
    self.storage = (0, 0)
    self._count = Int8(truncatingIfNeeded: count)
  }

  @inline(__always)
  internal init() {
    self.init(count: 0)
  }

  @inline(__always)
  internal init(allZeros: ()) {
    self.init(count: 2)
  }
}

extension _FixedArray2 {
  internal mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let count = self.count // for exclusive access
    return try withUnsafeMutableBytes(of: &storage) { rawBuffer in
      _internalInvariant(rawBuffer.count == 2*MemoryLayout<T>.stride,
        "layout mismatch?")
      let buffer = UnsafeMutableBufferPointer<Element>(
        start: rawBuffer.baseAddress._unsafelyUnwrappedUnchecked
          .assumingMemoryBound(to: Element.self),
        count: count)
      return try body(buffer)
    }
  }

  internal mutating func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let count = self.count // for exclusive access
    return try withUnsafeBytes(of: &storage) { rawBuffer in
      _internalInvariant(rawBuffer.count == 2*MemoryLayout<T>.stride,
        "layout mismatch?")
      let buffer = UnsafeBufferPointer<Element>(
        start: rawBuffer.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: Element.self),
        count: count)
      return try body(buffer)
    }
  }
}

internal struct _FixedArray4<T> {
  // ABI TODO: The has assumptions about tuple layout in the ABI, namely that
  // they are laid out contiguously and individually addressable (i.e. strided).
  //
  internal var storage: (
    // A 4-wide tuple of type T
    T, T, T, T
  )

  var _count: Int8
}

extension _FixedArray4 {
  internal static var capacity: Int {
    @inline(__always) get { return 4 }
  }

  internal var capacity: Int {
    @inline(__always) get { return 4 }
  }

  internal var count: Int {
    @inline(__always) get { return Int(truncatingIfNeeded: _count) }
    @inline(__always) set { _count = Int8(newValue) }
  }
}

extension _FixedArray4: RandomAccessCollection, MutableCollection {
  internal typealias Index = Int

  internal var startIndex: Index {
    return 0
  }

  internal var endIndex: Index {
    return count
  }

  internal subscript(i: Index) -> T {
    @inline(__always)
    get {
      let count = self.count // for exclusive access
      _internalInvariant(i >= 0 && i < count)
      let res: T = withUnsafeBytes(of: storage) {
        (rawPtr : UnsafeRawBufferPointer) -> T in
        let stride = MemoryLayout<T>.stride
        _internalInvariant(rawPtr.count == 4*stride, "layout mismatch?")
        let bufPtr = UnsafeBufferPointer(
          start: rawPtr.baseAddress!.assumingMemoryBound(to: T.self),
          count: count)
        return bufPtr[_unchecked: i]
      }
      return res
    }
    @inline(__always)
    set {
      _internalInvariant(i >= 0 && i < count)
      self.withUnsafeMutableBufferPointer { buffer in
        buffer[_unchecked: i] = newValue
      }
    }
  }

  @inline(__always)
  internal func index(after i: Index) -> Index {
    return i+1
  }

  @inline(__always)
  internal func index(before i: Index) -> Index {
    return i-1
  }
}

extension _FixedArray4 {
  internal mutating func append(_ newElement: T) {
    _internalInvariant(count < capacity)
    _count += 1
    self[count-1] = newElement
  }
}

extension _FixedArray4 where T: ExpressibleByIntegerLiteral {
  @inline(__always)
  internal init(count: Int) {
    _internalInvariant(count >= 0 && count <= _FixedArray4.capacity)
    self.storage = (0, 0, 0, 0)
    self._count = Int8(truncatingIfNeeded: count)
  }

  @inline(__always)
  internal init() {
    self.init(count: 0)
  }

  @inline(__always)
  internal init(allZeros: ()) {
    self.init(count: 4)
  }
}

extension _FixedArray4 {
  internal mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let count = self.count // for exclusive access
    return try withUnsafeMutableBytes(of: &storage) { rawBuffer in
      _internalInvariant(rawBuffer.count == 4*MemoryLayout<T>.stride,
        "layout mismatch?")
      let buffer = UnsafeMutableBufferPointer<Element>(
        start: rawBuffer.baseAddress._unsafelyUnwrappedUnchecked
          .assumingMemoryBound(to: Element.self),
        count: count)
      return try body(buffer)
    }
  }

  internal mutating func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let count = self.count // for exclusive access
    return try withUnsafeBytes(of: &storage) { rawBuffer in
      _internalInvariant(rawBuffer.count == 4*MemoryLayout<T>.stride,
        "layout mismatch?")
      let buffer = UnsafeBufferPointer<Element>(
        start: rawBuffer.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: Element.self),
        count: count)
      return try body(buffer)
    }
  }
}

internal struct _FixedArray8<T> {
  // ABI TODO: The has assumptions about tuple layout in the ABI, namely that
  // they are laid out contiguously and individually addressable (i.e. strided).
  //
  internal var storage: (
    // A 8-wide tuple of type T
    T, T, T, T, T, T, T, T
  )

  var _count: Int8
}

extension _FixedArray8 {
  internal static var capacity: Int {
    @inline(__always) get { return 8 }
  }

  internal var capacity: Int {
    @inline(__always) get { return 8 }
  }

  internal var count: Int {
    @inline(__always) get { return Int(truncatingIfNeeded: _count) }
    @inline(__always) set { _count = Int8(newValue) }
  }
}

extension _FixedArray8: RandomAccessCollection, MutableCollection {
  internal typealias Index = Int

  internal var startIndex: Index {
    return 0
  }

  internal var endIndex: Index {
    return count
  }

  internal subscript(i: Index) -> T {
    @inline(__always)
    get {
      let count = self.count // for exclusive access
      _internalInvariant(i >= 0 && i < count)
      let res: T = withUnsafeBytes(of: storage) {
        (rawPtr : UnsafeRawBufferPointer) -> T in
        let stride = MemoryLayout<T>.stride
        _internalInvariant(rawPtr.count == 8*stride, "layout mismatch?")
        let bufPtr = UnsafeBufferPointer(
          start: rawPtr.baseAddress!.assumingMemoryBound(to: T.self),
          count: count)
        return bufPtr[_unchecked: i]
      }
      return res
    }
    @inline(__always)
    set {
      _internalInvariant(i >= 0 && i < count)
      self.withUnsafeMutableBufferPointer { buffer in
        buffer[_unchecked: i] = newValue
      }
    }
  }

  @inline(__always)
  internal func index(after i: Index) -> Index {
    return i+1
  }

  @inline(__always)
  internal func index(before i: Index) -> Index {
    return i-1
  }
}

extension _FixedArray8 {
  internal mutating func append(_ newElement: T) {
    _internalInvariant(count < capacity)
    _count += 1
    self[count-1] = newElement
  }
}

extension _FixedArray8 where T: ExpressibleByIntegerLiteral {
  @inline(__always)
  internal init(count: Int) {
    _internalInvariant(count >= 0 && count <= _FixedArray8.capacity)
    self.storage = (0, 0, 0, 0, 0, 0, 0, 0)
    self._count = Int8(truncatingIfNeeded: count)
  }

  @inline(__always)
  internal init() {
    self.init(count: 0)
  }

  @inline(__always)
  internal init(allZeros: ()) {
    self.init(count: 8)
  }
}

extension _FixedArray8 {
  internal mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let count = self.count // for exclusive access
    return try withUnsafeMutableBytes(of: &storage) { rawBuffer in
      _internalInvariant(rawBuffer.count == 8*MemoryLayout<T>.stride,
        "layout mismatch?")
      let buffer = UnsafeMutableBufferPointer<Element>(
        start: rawBuffer.baseAddress._unsafelyUnwrappedUnchecked
          .assumingMemoryBound(to: Element.self),
        count: count)
      return try body(buffer)
    }
  }

  internal mutating func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let count = self.count // for exclusive access
    return try withUnsafeBytes(of: &storage) { rawBuffer in
      _internalInvariant(rawBuffer.count == 8*MemoryLayout<T>.stride,
        "layout mismatch?")
      let buffer = UnsafeBufferPointer<Element>(
        start: rawBuffer.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: Element.self),
        count: count)
      return try body(buffer)
    }
  }
}

internal struct _FixedArray16<T> {
  // ABI TODO: The has assumptions about tuple layout in the ABI, namely that
  // they are laid out contiguously and individually addressable (i.e. strided).
  //
  internal var storage: (
    // A 16-wide tuple of type T
    T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T
  )

  var _count: Int8
}

extension _FixedArray16 {
  internal static var capacity: Int {
    @inline(__always) get { return 16 }
  }

  internal var capacity: Int {
    @inline(__always) get { return 16 }
  }

  internal var count: Int {
    @inline(__always) get { return Int(truncatingIfNeeded: _count) }
    @inline(__always) set { _count = Int8(newValue) }
  }
}

extension _FixedArray16: RandomAccessCollection, MutableCollection {
  internal typealias Index = Int

  internal var startIndex: Index {
    return 0
  }

  internal var endIndex: Index {
    return count
  }

  internal subscript(i: Index) -> T {
    @inline(__always)
    get {
      let count = self.count // for exclusive access
      _internalInvariant(i >= 0 && i < count)
      let res: T = withUnsafeBytes(of: storage) {
        (rawPtr : UnsafeRawBufferPointer) -> T in
        let stride = MemoryLayout<T>.stride
        _internalInvariant(rawPtr.count == 16*stride, "layout mismatch?")
        let bufPtr = UnsafeBufferPointer(
          start: rawPtr.baseAddress!.assumingMemoryBound(to: T.self),
          count: count)
        return bufPtr[_unchecked: i]
      }
      return res
    }
    @inline(__always)
    set {
      _internalInvariant(i >= 0 && i < count)
      self.withUnsafeMutableBufferPointer { buffer in
        buffer[_unchecked: i] = newValue
      }
    }
  }

  @inline(__always)
  internal func index(after i: Index) -> Index {
    return i+1
  }

  @inline(__always)
  internal func index(before i: Index) -> Index {
    return i-1
  }
}

extension _FixedArray16 {
  internal mutating func append(_ newElement: T) {
    _internalInvariant(count < capacity)
    _count += 1
    self[count-1] = newElement
  }
}

extension _FixedArray16 where T : ExpressibleByIntegerLiteral {
  @inline(__always)
  internal init(count: Int) {
    _internalInvariant(count >= 0 && count <= _FixedArray16.capacity)
    self.storage = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    self._count = Int8(truncatingIfNeeded: count)
  }

  @inline(__always)
  internal init() {
    self.init(count: 0)
  }

  @inline(__always)
  internal init(allZeros: ()) {
    self.init(count: 16)
  }
}

extension _FixedArray16 {
  internal mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let count = self.count // for exclusive access
    return try withUnsafeMutableBytes(of: &storage) { rawBuffer in
      _internalInvariant(rawBuffer.count == 16*MemoryLayout<T>.stride,
        "layout mismatch?")
      let buffer = UnsafeMutableBufferPointer<Element>(
        start: rawBuffer.baseAddress._unsafelyUnwrappedUnchecked
          .assumingMemoryBound(to: Element.self),
        count: count)
      return try body(buffer)
    }
  }

  internal mutating func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    let count = self.count // for exclusive access
    return try withUnsafeBytes(of: &storage) { rawBuffer in
      _internalInvariant(rawBuffer.count == 16*MemoryLayout<T>.stride,
        "layout mismatch?")
      let buffer = UnsafeBufferPointer<Element>(
        start: rawBuffer.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: Element.self),
        count: count)
      return try body(buffer)
    }
  }
}
