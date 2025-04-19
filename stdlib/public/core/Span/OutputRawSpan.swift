//===--- OutputRawSpan.swift ----------------------------------------------===//
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

// OutputRawSpan is a reference to a contiguous region of memory which starts
// some number of initialized bytes, followed by uninitialized memory.
@safe
@frozen
@available(SwiftStdlib 6.2, *)
public struct OutputRawSpan: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  public let capacity: Int

  @usableFromInline
  internal var _count: Int

  @_alwaysEmitIntoClient
  @lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    capacity = 0
    _count = 0
  }

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
  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  @_alwaysEmitIntoClient
  @_transparent
  internal func _tail() -> UnsafeMutableRawPointer {
    // NOTE: `_pointer` must be known to be not-nil.
    unsafe _start().advanced(by: _count)
  }

  @_alwaysEmitIntoClient
  public var freeCapacity: Int { capacity &- _count }

  @_alwaysEmitIntoClient
  public var count: Int { _count }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _count == 0 }

  @_alwaysEmitIntoClient
  public var isFull: Bool { _count == capacity }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  internal init(
    _uncheckedBuffer buffer: UnsafeMutableRawBufferPointer,
    initializedCount: Int
  ) {
    unsafe _pointer = .init(buffer.baseAddress)
    capacity = buffer.count
    _count = initializedCount
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    buffer: UnsafeMutableRawBufferPointer,
    initializedCount: Int
  ) {
    if let baseAddress = buffer.baseAddress {
      _precondition(
        unsafe baseAddress.advanced(by: buffer.count) >= baseAddress,
        "Buffer must not wrap around the address space"
      )
    }
    _precondition(
      0 <= initializedCount && initializedCount <= buffer.count,
      "OutputSpan count is not within capacity"
    )
    unsafe self.init(
      _uncheckedBuffer: buffer, initializedCount: initializedCount
    )
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    buffer: borrowing Slice<UnsafeMutableRawBufferPointer>,
    initializedCount: Int
  ) {
    let rebased = unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
    let os = unsafe OutputRawSpan(
      buffer: rebased, initializedCount: initializedCount
    )
    self = unsafe _overrideLifetime(os, borrowing: buffer)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(_ value: UInt8) {
    _precondition(_count < capacity, "OutputRawSpan capacity overflow")
    unsafe _tail().storeBytes(of: value, as: UInt8.self)
    _count &+= 1
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeLast() -> UInt8? {
    guard _count > 0 else { return nil }
    _count &-= 1
    return unsafe _tail().load(as: UInt8.self)
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeAll() {
    // TODO: Consider an option to zero the `_count` bytes being removed.
    _count = 0
  }
}

//MARK: bulk-append functions
@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(repeating repeatedValue: UInt8, count: Int) {
    _precondition(count <= freeCapacity, "OutputRawSpan capacity overflow")
    unsafe _tail().initializeMemory(
      as: UInt8.self, repeating: repeatedValue, count: count
    )
    _count &+= count
  }

  /// Returns true if the iterator has filled all the free capacity in the span.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  @discardableResult
  public mutating func append(
    from elements: inout some IteratorProtocol<UInt8>
  ) -> Bool {
    // FIXME: It may be best to delay this API until
    //        we have designed a chunking IteratorProtocol
    var p = unsafe _tail()
    while _count < capacity {
      guard let byte = elements.next() else { return false }
      unsafe p.initializeMemory(as: UInt8.self, to: byte)
      unsafe p = p.advanced(by: MemoryLayout<UInt8>.stride)
      _count &+= 1
    }
    return true
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: some Collection<UInt8>
  ) {
    let done: Void? = source.withContiguousStorageIfAvailable {
      unsafe append(fromContentsOf: $0)
    }
    if done != nil {
      return
    }

    let freeCapacity = freeCapacity
    var (iterator, copied) = unsafe _tail().withMemoryRebound(
      to: UInt8.self, capacity: freeCapacity
    ) {
      let suffix = unsafe UnsafeMutableBufferPointer(
        start: $0, count: freeCapacity
      )
      return unsafe source._copyContents(initializing: suffix)
    }
    _precondition(iterator.next() == nil, "OutputRawSpan capacity overflow")
    _precondition(_count + copied <= capacity, "Invalid Sequence._copyContents")
    _count &+= copied
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: UnsafeRawBufferPointer
  ) {
    guard unsafe !source.isEmpty else { return }
    let addedBytes = source.count
    _precondition(addedBytes <= freeCapacity, "OutputRawSpan capacity overflow")
    unsafe _tail().copyMemory(from: source.baseAddress!, byteCount: addedBytes)
    _count += addedBytes
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: Span<UInt8>
  ) {
    unsafe source.withUnsafeBytes { unsafe append(fromContentsOf: $0) }
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

  /// Call the given closure with the unsafe buffer pointer addressed by this
  /// OutputRawSpan and a mutable reference to its count of initialized bytes.
  ///
  /// This method provides a way for Swift code to process or populate an
  /// `OutputRawSpan` using unsafe operations; for example, it allows
  /// dispatching to code written in legacy (memory-unsafe) languages.
  ///
  /// The supplied function may process the buffer in any way it wants; however,
  /// when it finishes (whether by returning or throwing), it must leave the
  /// buffer in a state that satisfies the invariants of the output span:
  ///
  /// 1. The inout integer passed in as the second argument must be the exact
  ///     number of initialized bytes in the buffer passed in as the first
  ///     argument.
  /// 2. These initialized elements must be located in a single contiguous
  ///     region starting at the beginning of the buffer. The rest of the buffer
  ///     must hold uninitialized memory.
  ///
  /// This function cannot verify these two invariants, and therefore
  /// this is an unsafe operation. Violating the invariants of `OutputRawSpan`
  /// may result in undefined behavior.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func withUnsafeMutableBytes<E: Error, R: ~Copyable>(
    _ body: (
      UnsafeMutableRawBufferPointer,
      _ initializedCount: inout Int
    ) throws(E) -> R
  ) throws(E) -> R {
    guard let start = unsafe _pointer, capacity > 0 else {
      let buffer = UnsafeMutableRawBufferPointer(_empty: ())
      var initializedCount = 0
      defer {
        _precondition(initializedCount == 0, "OutputRawSpan capacity overflow")
      }
      return unsafe try body(buffer, &initializedCount)
    }
    let buffer = unsafe UnsafeMutableRawBufferPointer(
      _uncheckedStart: start, count: capacity
    )
    var initializedCount = _count
    defer {
      _precondition(
        0 <= initializedCount && initializedCount <= capacity,
        "OutputRawSpan capacity overflow"
      )
      _count = initializedCount
    }
    return unsafe try body(buffer, &initializedCount)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {
  /// Consume the output span (relinquishing its control over the buffer it is
  /// addressing), and return the number of initialized bytes in it.
  ///
  /// This method should be invoked in the scope where the `OutputRawSpan` was
  /// created, when it is time to commit the contents of the updated buffer
  /// back into the construct being initialized.
  ///
  /// The context that created the output span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer we expect the `OutputRawSpan` to reference.
  ///      This must be the same region of memory passed to
  ///      the `OutputRawSpan` initializer.
  /// - Returns: The number of initialized bytes in the same buffer, as
  ///      tracked by the consumed `OutputRawSpan` instance.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalize(
    for buffer: UnsafeMutableRawBufferPointer
  ) -> Int {
    _precondition(
      unsafe buffer.baseAddress == self._pointer
      && buffer.count == self.capacity,
      "OutputRawSpan identity mismatch")
    return _count
  }

  /// Consume the output span (relinquishing its control over the buffer it is
  /// addressing), and return the number of initialized bytes in it.
  ///
  /// This method should be invoked in the scope where the `OutputRawSpan` was
  /// created, when it is time to commit the contents of the updated buffer
  /// back into the construct being initialized.
  ///
  /// The context that created the output span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer we expect the `OutputRawSpan` to reference.
  ///      This must be the same region of memory passed to
  ///      the `OutputRawSpan` initializer.
  /// - Returns: The number of initialized bytes in the same buffer, as
  ///      tracked by the consumed `OutputRawSpan` instance.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalize(
    for buffer: Slice<UnsafeMutableRawBufferPointer>
  ) -> Int {
    let rebased = unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
    return unsafe self.finalize(for: rebased)
  }
}
