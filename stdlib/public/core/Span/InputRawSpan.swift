//===--- InputRawSpan.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// `InputRawSpan` is a reference to a contiguous region of memory that starts
/// with uninitialized memory, followed by some number of initialized bytes.
/// It provides operations to access the bytes it stores, as well as adding
/// and removing bytes from the front of its initialized region.
@safe
@frozen
@available(SwiftStdlib 6.5, *)
public struct InputRawSpan: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  /// The total number of bytes that this input span can contain.
  @usableFromInline
  internal let _capacity: Int

  @usableFromInline
  internal var _count: Int

  /// Create an InputRawSpan with zero capacity.
  @export(implementation)
  @_lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    _capacity = 0
    _count = 0
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan: @unchecked Sendable {}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan {
  /// Returns the address of the start of the span's underlying buffer
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  /// Returns the address of the first initialized byte. Bytes that precede
  /// it, up to the start of the buffer, are uninitialized.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  internal func _front() -> UnsafeMutableRawPointer {
    unsafe _start().advanced(by: capacity &- byteCount)
  }

  /// The address of the first initialized byte, or `nil` if this span
  /// does not reference any memory.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  internal func _initializedStart() -> UnsafeMutableRawPointer? {
    unsafe _pointer?.advanced(by: capacity &- byteCount)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan {
  /// The number of initialized bytes in this span.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_semantics("fixed_storage.get_count")
  public var byteCount: Int { _assumeNonNegative(_count) }

  @available(SwiftStdlib 6.5, *)
  @export(implementation) @inline(always)
  public var capacity: Int { _assumeNonNegative(_capacity) }

  /// The number of additional bytes that can be prepended to this span.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var freeCapacity: Int {
    _assumeNonNegative(capacity &- byteCount)
  }

  /// A Boolean value indicating whether the span is empty.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var isEmpty: Bool { byteCount == 0 }

  /// A Boolean value indicating whether the span is full.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var isFull: Bool { byteCount == capacity }

  /// The indices that are valid for subscripting the span, in ascending
  /// order.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public var byteOffsets: Range<Int> {
    unsafe Range(_uncheckedBounds: (0, byteCount))
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan {

  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  internal init(
    _uncheckedBuffer buffer: UnsafeMutableRawBufferPointer,
    initializedCount: Int
  ) {
    unsafe _pointer = .init(buffer.baseAddress)
    _capacity = buffer.count
    _count = initializedCount
  }

  /// Unsafely create an InputRawSpan over partly-initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime
  /// of the newly-created `InputRawSpan`. Its suffix must contain
  /// `initializedCount` initialized bytes, preceded by uninitialized
  /// memory.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableRawBufferPointer` with some number of
  ///       initialized bytes, all compacted into the end of the buffer.
  ///   - initializedCount: the number of initialized bytes
  ///       at the end of `buffer`.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(
    buffer: UnsafeMutableRawBufferPointer,
    initializedCount: Int
  ) {
    if let baseAddress = buffer.baseAddress {
      _precondition(
        unsafe baseAddress <= baseAddress.advanced(by: buffer.count),
        "Buffer must not wrap around the address space."
      )
    }
    _precondition(
      UInt(bitPattern: initializedCount) <= UInt(bitPattern: buffer.count),
      "InputRawSpan count is invalid or greater than capacity."
    )
    unsafe self.init(
      _uncheckedBuffer: buffer, initializedCount: initializedCount
    )
  }

  /// Unsafely create an InputRawSpan over partly-initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime
  /// of the newly-created `InputRawSpan`. Its suffix must contain
  /// `initializedCount` initialized bytes, preceded by uninitialized
  /// memory.
  ///
  /// - Parameters:
  ///   - buffer: a slice of an `UnsafeMutableRawBufferPointer` with some
  ///       number of initialized bytes, all compacted into the end of
  ///       the slice.
  ///   - initializedCount: the number of initialized bytes
  ///       at the end of `buffer`.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(
    buffer: borrowing Slice<UnsafeMutableRawBufferPointer>,
    initializedCount: Int
  ) {
    let rebased = unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
    let span = unsafe InputRawSpan(
      buffer: rebased, initializedCount: initializedCount
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan {

  /// Prepend a single byte to this span.
  ///
  /// - Parameter value: The byte to prepend.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func prepend(_ value: UInt8) {
    unsafe _prepend(value, as: UInt8.self)
  }

  /// Remove the first byte from this span.
  ///
  /// Returns the first byte. The `InputRawSpan` must not be empty.
  ///
  /// - Returns: The removed byte.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func removeFirst() -> UInt8 {
    _precondition(!isEmpty, "InputRawSpan underflow")
    let byte = unsafe _front().load(as: UInt8.self)
    _count &-= 1
    return byte
  }

  /// Remove the first n bytes from this span, returning the memory
  /// they occupy to the uninitialized state.
  ///
  /// `n` must not be greater than `byteCount`.
  ///
  /// - Parameter n: The number of bytes to remove.
  ///     `n` must not be negative or greater than `byteCount`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func removeFirst(_ n: Int) {
    _precondition(n >= 0, "Can't remove a negative number of bytes")
    _precondition(n <= byteCount, "InputRawSpan underflow")
    _count &-= n
  }

  /// Removes and returns the first byte of this span, if it exists.
  ///
  /// - Returns: The first byte of the span if the span isn't empty;
  ///    otherwise, `nil`.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func popFirst() -> UInt8? {
    guard !isEmpty else { return nil }
    return removeFirst()
  }

  /// Remove all this span's bytes and return its memory
  /// to the uninitialized state.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func removeAll() {
    _count = 0
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan {
  // Can we use: @_semantics("fixed_storage.check_index")
  @available(SwiftStdlib 6.5, *)
  @export(implementation) @inline(__always)
  internal func _checkIndex(_ position: Int) {
    _precondition(
      UInt(bitPattern: position) < UInt(bitPattern: byteCount),
      "Index out of bounds"
    )
  }

  /// Accesses the byte at the specified offset in the span.
  ///
  /// - Parameter byteOffset: The offset of the byte to access. `byteOffset`
  ///     must be greater than or equal to zero, and less than `byteCount`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation) @inline(__always)
  public subscript(_ byteOffset: Int) -> UInt8 {
    get {
      _checkIndex(byteOffset)
      return unsafe self[unchecked: byteOffset]
    }
    set {
      _checkIndex(byteOffset)
      unsafe self[unchecked: byteOffset] = newValue
    }
  }

  /// Accesses the byte at the specified offset in the span.
  ///
  /// This subscript does not validate `byteOffset`. Using this subscript
  /// with an invalid `byteOffset` results in undefined behaviour.
  ///
  /// - Parameter byteOffset: The offset of the byte to access. `byteOffset`
  ///     must be greater than or equal to zero, and less than `byteCount`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation) @inline(__always)
  @unsafe
  public subscript(unchecked byteOffset: Int) -> UInt8 {
    get {
      unsafe _front().load(fromByteOffset: byteOffset, as: UInt8.self)
    }
    set {
      unsafe _front().storeBytes(
        of: newValue, toByteOffset: byteOffset, as: UInt8.self
      )
    }
  }
}

// MARK: generic single-element prepend functions
@available(SwiftStdlib 6.5, *)
extension InputRawSpan {
  /// Prepends the given value's bytes to this span's bytes.
  ///
  /// Some of the bytes prepended may be padding whose contents was never
  /// explicitly set by the running program.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - type: The type of the value.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @unsafe
  @_lifetime(self: copy self)
  public mutating func prepend<T: BitwiseCopyable>(
    paddedBytes value: T, as type: T.Type
  ) {
    unsafe _prepend(value, as: T.self)
  }

  /// Prepends the given value's bytes to this span's bytes.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  @_lifetime(self: copy self)
  internal mutating func _prepend<T>(
    _ value: T, as type: T.Type
  ) where T: BitwiseCopyable {
    let size = MemoryLayout<T>.size
    _precondition(size <= freeCapacity, "InputRawSpan capacity overflow")
    guard size > 0 else { return }
    let address = unsafe _front().advanced(by: 0 &- size)
    unsafe address.initializeMemory(as: T.self, to: value)
    _count &+= size
  }

  /// Prepends the given value's bytes to this span's bytes.
  ///
  /// There must be at least `MemoryLayout<T>.size` bytes available
  /// in the span.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - type: The type of the instance to store.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func prepend<T>(
    _ value: T, as type: T.Type
  ) where T: ConvertibleToBytes & BitwiseCopyable {
    unsafe _prepend(value, as: T.self)
  }

  /// Prepends the given value's bytes to this span's bytes.
  ///
  /// There must be at least `MemoryLayout<T>.size` bytes available
  /// in the span.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - type: The type of the instance to store.
  ///   - byteOrder: The order in which the bytes will be encoded to the span.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func prepend<T>(
    _ value: T, as type: T.Type, _ byteOrder: ByteOrder
  ) where T: ConvertibleToBytes & BitwiseCopyable & FixedWidthInteger {
    let rawValue = switch byteOrder {
    case .bigEndian: value.bigEndian
    case .littleEndian: value.littleEndian
    }
    unsafe _prepend(rawValue, as: T.self)
  }
}

// MARK: bulk-prepend functions
@available(SwiftStdlib 6.5, *)
extension InputRawSpan {
  /// Prepends the given value's bytes repeatedly to this span's bytes.
  ///
  /// Some of the bytes prepended may be padding whose contents was never
  /// explicitly set by the running program.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to prepend
  ///       to this span.
  ///   - type: The type of the instance to store repeatedly.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @unsafe
  @_lifetime(self: copy self)
  public mutating func prepend<T: BitwiseCopyable>(
    repeatingPaddedBytes repeatedValue: T,
    count: Int,
    as type: T.Type
  ) {
    unsafe _prepend(repeating: repeatedValue, count: count, as: T.self)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  @_lifetime(self: copy self)
  internal mutating func _prepend<T: BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    _precondition(count >= 0, "Can't prepend a negative number of values")
    let total = count * MemoryLayout<T>.stride
    _precondition(total <= freeCapacity, "InputRawSpan capacity overflow")
    guard count > 0 else { return }
    unsafe _start().advanced(
      by: capacity &- byteCount &- total
    ).initializeMemory(
      as: T.self, repeating: repeatedValue, count: count
    )
    _count &+= total
  }

  /// Prepends the given value's bytes repeatedly to this span's bytes.
  ///
  /// There must be at least `count * MemoryLayout<T>.stride` bytes
  /// available in the span.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to prepend
  ///       to this span.
  ///   - type: The type of the instance to store repeatedly.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func prepend<T>(
    repeating repeatedValue: T,
    count: Int,
    as type: T.Type
  ) where T: ConvertibleToBytes & BitwiseCopyable {
    unsafe _prepend(repeating: repeatedValue, count: count, as: T.self)
  }

  /// Prepends the given value's bytes repeatedly to this span's bytes.
  ///
  /// There must be at least `count * MemoryLayout<T>.stride` bytes
  /// available in the span.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to prepend
  ///       to this span.
  ///   - type: The type of the instance to store repeatedly.
  ///   - byteOrder: The order in which the bytes will be encoded to the span.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func prepend<T>(
    repeating repeatedValue: T,
    count: Int,
    as type: T.Type,
    _ byteOrder: ByteOrder
  ) where T: ConvertibleToBytes & BitwiseCopyable & FixedWidthInteger {
    let rawValue = switch byteOrder {
    case .bigEndian: repeatedValue.bigEndian
    case .littleEndian: repeatedValue.littleEndian
    }
    unsafe _prepend(repeating: rawValue, count: count, as: T.self)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan {
  /// Borrow the underlying initialized memory for read-only access.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var bytes: RawSpan {
    @_lifetime(borrow self)
    borrowing get {
      let span = unsafe RawSpan(
        _unchecked: _initializedStart(), byteCount: byteCount
      )
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  /// Exclusively borrow the underlying initialized memory for mutation.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var mutableBytes: MutableRawSpan {
    @_lifetime(&self)
    mutating get {
      let span = unsafe MutableRawSpan(
        _unchecked: _initializedStart(), byteCount: byteCount
      )
      return unsafe _overrideLifetime(span, mutating: &self)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan {

  /// Call the given closure with the unsafe buffer pointer addressed by this
  /// InputRawSpan and a mutable reference to its count of initialized bytes.
  ///
  /// This method provides a way to process or consume the contents of an
  /// `InputRawSpan` using unsafe operations, such as dispatching to code
  /// written in legacy (memory-unsafe) languages.
  ///
  /// The supplied closure may process the buffer in any way it wants; however,
  /// when it finishes (whether by returning or throwing), it must leave the
  /// buffer in a state that satisfies the invariants of the input span:
  ///
  /// 1. The inout integer passed in as the second argument must be the exact
  ///     number of initialized bytes in the buffer passed in as the first
  ///     argument.
  /// 2. These initialized bytes must be located in a single contiguous
  ///     region located at the end of the buffer. The initialized region must
  ///     be preceded by uninitialized memory.
  ///
  /// This function cannot verify these two invariants, and therefore
  /// this is an unsafe operation. Violating the invariants of `InputRawSpan`
  /// may result in undefined behavior.
  ///
  /// - Parameter body: A closure that can read from and write to the
  ///     buffer and update the initialized count.
  /// - Returns: The return value of the `body` closure.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @_lifetime(self: copy self)
  @unsafe
  public mutating func withUnsafeMutableBytes<E: Error, R: ~Copyable>(
    _ body: (
      UnsafeMutableRawBufferPointer,
      _ initializedCount: inout Int
    ) throws(E) -> R
  ) throws(E) -> R {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _pointer, count: capacity
    )
    var initializedCount = byteCount
    defer {
      _precondition(
        UInt(bitPattern: initializedCount) <= UInt(bitPattern: capacity),
        "InputRawSpan capacity overflow"
      )
      _count = initializedCount
    }
    return unsafe try body(bytes, &initializedCount)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan {
  /// Consume the input span and return the number of initialized bytes
  /// remaining at the end of the underlying memory region.
  ///
  /// This method should be invoked in the scope where the `InputRawSpan` was
  /// created, when it is time to commit the contents of the updated buffer
  /// back into the construct that was accessed.
  ///
  /// The context that created the input span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer we expect the `InputRawSpan` to reference.
  ///      This must be the same region of memory passed to
  ///      the `InputRawSpan` initializer.
  /// - Returns: The number of initialized bytes remaining at the end of
  ///      the same buffer, as tracked by the consumed `InputRawSpan` instance.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  public consuming func finalize(
    for buffer: UnsafeMutableRawBufferPointer
  ) -> Int {
    _precondition(
      unsafe buffer.baseAddress == _pointer
      && buffer.count == capacity,
      "InputRawSpan identity mismatch")
    return byteCount
  }

  /// Consume the input span and return the number of initialized bytes
  /// remaining at the end of the underlying memory region.
  ///
  /// This method should be invoked in the scope where the `InputRawSpan` was
  /// created, when it is time to commit the contents of the updated buffer
  /// back into the construct that was accessed.
  ///
  /// The context that created the input span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer we expect the `InputRawSpan` to reference.
  ///      This must be the same region of memory passed to
  ///      the `InputRawSpan` initializer.
  /// - Returns: The number of initialized bytes remaining at the end of
  ///      the same buffer, as tracked by the consumed `InputRawSpan` instance.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  public consuming func finalize(
    for buffer: Slice<UnsafeMutableRawBufferPointer>
  ) -> Int {
    let rebased = unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
    return unsafe self.finalize(for: rebased)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputRawSpan: Iterable {
  @available(SwiftStdlib 6.5, *)
  public typealias BorrowingIterator = Span<UInt8>.BorrowingIterator

  @available(SwiftStdlib 6.5, *)
  public typealias Failure = Never

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public var underestimatedCount: Int {
    self.byteCount
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<UInt8>.BorrowingIterator {
    .init(Span(viewing: self.bytes))
  }
}
