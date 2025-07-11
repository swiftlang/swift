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

  /// Create an OutputRawSpan with zero capacity
  @_alwaysEmitIntoClient
  @lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    capacity = 0
    _count = 0
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan: @unchecked Sendable {}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {
  @_alwaysEmitIntoClient
  @_transparent
  @unsafe
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  @_alwaysEmitIntoClient
  @_transparent
  @unsafe
  internal func _tail() -> UnsafeMutableRawPointer {
    // NOTE: `_pointer` must be known to be not-nil.
    unsafe _start().advanced(by: _count)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {
  /// The number of initialized bytes in this span.
  @_alwaysEmitIntoClient
  public var byteCount: Int { _count }

  /// The number of additional bytes that can be appended to this span.
  @_alwaysEmitIntoClient
  public var freeCapacity: Int { capacity &- _count }

  /// A Boolean value indicating whether the span is empty.
  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _count == 0 }

  /// A Boolean value indicating whether the span is full.
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

  /// Unsafely create an OutputRawSpan over partly-initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime
  /// of the newly-created `OutputRawSpan`. Its prefix must contain
  /// `initializedCount` initialized bytes, followed by uninitialized
  /// memory.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to be initialized
  ///   - initializedCount: the number of initialized bytes
  ///                       at the beginning of `buffer`.
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

  /// Unsafely create an OutputRawSpan over partly-initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime
  /// of the newly-created `OutputRawSpan`. Its prefix must contain
  /// `initializedCount` initialized bytes, followed by uninitialized
  /// memory.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to be initialized
  ///   - initializedCount: the number of initialized bytes
  ///                       at the beginning of `buffer`.
  @unsafe
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

  /// Append a single byte to this span.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(_ value: UInt8) {
    _precondition(_count < capacity, "OutputRawSpan capacity overflow")
    unsafe _tail().storeBytes(of: value, as: UInt8.self)
    _count &+= 1
  }

  /// Remove the last byte from this span.
  @_alwaysEmitIntoClient
  @discardableResult
  @lifetime(self: copy self)
  public mutating func removeLast() -> UInt8 {
    _precondition(!isEmpty, "OutputRawSpan underflow")
    _count &-= 1
    return unsafe _tail().load(as: UInt8.self)
  }

  /// Remove the last N elements, returning the memory they occupy
  /// to the uninitialized state.
  ///
  /// `n` must not be greater than `count`
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeLast(_ k: Int) {
    _precondition(k >= 0, "Can't remove a negative number of bytes")
    _precondition(k <= _count, "OutputRawSpan underflow")
    _count &-= k
  }

  /// Remove all this span's elements and return its memory
  /// to the uninitialized state.
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
  /// Appends the given value's bytes to this span's bytes.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append<T: BitwiseCopyable>(_ value: T, as type: T.Type) {
    _precondition(
      MemoryLayout<T>.size <= freeCapacity, "OutputRawSpan capacity overflow"
    )
    unsafe _tail().initializeMemory(as: T.self, to: value)
    _count &+= MemoryLayout<T>.size
  }

  /// Appends the given value's bytes repeatedly to this span's bytes.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append<T: BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    let total = count * MemoryLayout<T>.stride
    _precondition(total <= freeCapacity, "OutputRawSpan capacity overflow")
    unsafe _tail().initializeMemory(
      as: T.self, repeating: repeatedValue, count: count
    )
    _count &+= total
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputRawSpan {
  /// Borrow the underlying initialized memory for read-only access.
  @_alwaysEmitIntoClient
  public var bytes: RawSpan {
    @lifetime(borrow self)
    borrowing get {
      let buffer = unsafe UnsafeRawBufferPointer(start: _pointer, count: _count)
      let span = unsafe RawSpan(_unsafeBytes: buffer)
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  /// Exclusively borrow the underlying initialized memory for mutation.
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
  /// This method provides a way to process or populate an `OutputRawSpan` using
  /// unsafe operations, such as dispatching to code written in legacy
  /// (memory-unsafe) languages.
  ///
  /// The supplied closure may process the buffer in any way it wants; however,
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
