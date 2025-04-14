//===--- OutputRawSpan.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// OutputRawSpan represents a span of memory which contains
// a variable number of initialized bytes, followed by uninitialized memory.
@safe
@frozen
@available(SwiftStdlib 6.2, *)
public struct OutputRawSpan: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  public let capacity: Int

  @usableFromInline
  internal var _count: Int = 0

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  @_alwaysEmitIntoClient
  public var freeCapacity: Int { capacity &- _count }

  @_alwaysEmitIntoClient
  public var count: Int { _count }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _count == 0 }

  @_alwaysEmitIntoClient
  public var isFull: Bool { _count == capacity }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow start)
  internal init(
    _unchecked start: UnsafeMutableRawPointer?,
    capacity: Int,
    initializedCount: Int
  ) {
    unsafe _pointer = start
    self.capacity = capacity
    _count = initializedCount
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan: @unchecked Sendable {}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  internal init(
    _unchecked buffer: UnsafeMutableRawBufferPointer,
    initialized: Int
  ) {
    unsafe _pointer = .init(buffer.baseAddress)
    capacity = buffer.count
    _count = initialized
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    buffer: UnsafeMutableRawBufferPointer,
    initialized: Int = 0
  ) {
    unsafe self.init(_unchecked: buffer, initialized: initialized)
  }

//  @_alwaysEmitIntoClient
//  @lifetime(borrow buffer)
//  public init(
//    _initializing buffer: borrowing Slice<UnsafeMutableRawBufferPointer>,
//    initialized: Int = 0
//  ) {
//    let rebased = unsafe UnsafeMutableBufferPointer(rebasing: buffer)
//    let os = OutputRawSpan(_initializing: rebased, initialized: 0)
//    self = unsafe _overrideLifetime(os, borrowing: buffer)
//  }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(_ value: consuming UInt8) {
    _precondition(_count < capacity, "Output buffer overflow")
    let p = unsafe _start().advanced(by: _count&*MemoryLayout<UInt8>.stride)
    unsafe p.initializeMemory(as: UInt8.self, to: value)
    _count &+= 1
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeLast() -> UInt8? {
    guard _count > 0 else { return nil }
    _count &-= 1
    let p = unsafe _start().advanced(by: _count&*MemoryLayout<UInt8>.stride)
    return unsafe p.withMemoryRebound(to: UInt8.self, capacity: 1, { unsafe $0.move() })
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeAll() {
    _ = unsafe _start().withMemoryRebound(to: UInt8.self, capacity: _count) {
      unsafe $0.deinitialize(count: _count)
    }
    _count = 0
  }
}

//MARK: bulk-update functions
@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(repeating repeatedValue: UInt8, count: Int) {
    let freeCapacity = capacity &- _count
    _precondition(
      count <= freeCapacity,
      "destination span cannot contain number of elements requested."
    )
    let offset = _count&*MemoryLayout<UInt8>.stride
    let p = unsafe _start().advanced(by: offset)
    unsafe p.withMemoryRebound(to: UInt8.self, capacity: count) {
      unsafe $0.initialize(repeating: repeatedValue, count: count)
    }
    _count &+= count
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append<S: Sequence>(
    from elements: S
  ) -> S.Iterator where S.Element == UInt8 {
    var iterator = elements.makeIterator()
    append(from: &iterator)
    return iterator
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    from elements: inout some IteratorProtocol<UInt8>
  ) {
    while _count < capacity {
      guard let byte = elements.next() else { break }
      let p = unsafe _start().advanced(by: _count&*MemoryLayout<UInt8>.stride)
      unsafe p.initializeMemory(as: UInt8.self, to: byte)
      _count &+= 1
    }
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: some Collection<UInt8>
  ) {
    let void: Void? = source.withContiguousStorageIfAvailable {
      append(fromContentsOf: unsafe Span(_unsafeElements: $0))
    }
    if void != nil {
      return
    }

    let freeCapacity = capacity &- _count
    let tail = unsafe _start().advanced(by: _count&*MemoryLayout<UInt8>.stride)
    var (iterator, copied) =
    unsafe tail.withMemoryRebound(to: UInt8.self, capacity: freeCapacity) {
      let suffix = unsafe UnsafeMutableBufferPointer(start: $0, count: freeCapacity)
      return unsafe source._copyContents(initializing: suffix)
    }
    _precondition(
      iterator.next() == nil,
      "destination span cannot contain every UInt8 from source."
    )
    assert(_count + copied <= capacity) // invariant check
    _count &+= copied
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: Span<UInt8>
  ) {
    guard !source.isEmpty else { return }
    _precondition(
      source.count <= freeCapacity,
      "destination span cannot contain every UInt8 from source."
    )
    let tail = unsafe _start().advanced(by: _count&*MemoryLayout<UInt8>.stride)
    _ = unsafe source.withUnsafeBufferPointer {
      unsafe tail.initializeMemory(
        as: UInt8.self, from: $0.baseAddress!, count: $0.count
      )
    }
    _count += source.count
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: borrowing MutableRawSpan
  ) {
    unsafe source.withUnsafeBytes { unsafe append(fromContentsOf: $0) }
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {

  @_alwaysEmitIntoClient
  public var bytes: RawSpan {
    @lifetime(borrow self)
    borrowing get {
      let buffer = unsafe UnsafeRawBufferPointer(start: _pointer, count: _count)
      let span = unsafe RawSpan(_unsafeBytes: buffer)
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  @_alwaysEmitIntoClient
  public var mutableBytes: MutableRawSpan {
    @lifetime(&self)
    mutating get {
      let buffer = unsafe UnsafeMutableRawBufferPointer(
        start: _pointer, count: _count
      )
      let span = unsafe MutableRawSpan(_unsafeBytes: buffer)
      return unsafe _overrideLifetime(span, mutating: &self)
    }
  }
}


@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {
  /// Consume the output span (relinquishing its control over the buffer it is
  /// addressing), and return the number of initialized bytes in it.
  ///
  /// This method is designed to be invoked in the same context that created the
  /// output span, when it is time to commit the contents of the updated buffer
  /// back into the construct that it came from.
  ///
  /// The context that created the output span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer that we the `OutputSpan` is expected to
  ///      relinquish using. This must be the same buffer as the one used to
  ///      originally initialize the `OutputSpan` instance.
  /// - Returns: The number of initialized bytes in the same buffer, as
  ///      tracked by the consumed `OutputSpan` instance.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalizeBytes(
    for buffer: UnsafeMutableRawBufferPointer
  ) -> Int {
    _precondition(
      unsafe buffer.baseAddress == self._pointer
      && buffer.count == self.capacity,
      "OutputRawSpan identity mismatch")
    return _count
  }
}
