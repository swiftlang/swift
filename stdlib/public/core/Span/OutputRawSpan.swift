//===--- OutputRawSpan.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SPAN_COMPATIBILITY_STUB
import Swift
#endif

/// `OutputRawSpan` is a reference to a contiguous region of memory which starts
/// with some number of initialized bytes, followed by uninitialized memory.
/// It provides operations to access the bytes it stores,
/// as well as to append and to remove bytes.
@safe
@frozen
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
public struct OutputRawSpan: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  /// The total number of bytes that this output span can contain.
  public let capacity: Int

  @usableFromInline
  internal var _count: Int

  /// Create an OutputRawSpan with zero capacity.
  @_alwaysEmitIntoClient
  @lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    capacity = 0
    _count = 0
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan: @unchecked Sendable {}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
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

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan {
  /// The number of initialized bytes in this span.
  @_alwaysEmitIntoClient
  @_semantics("fixed_storage.get_count")
  public var byteCount: Int { _assumeNonNegative(_count) }

  /// The number of additional bytes that can be appended to this span.
  @_alwaysEmitIntoClient
  @_transparent
  public var freeCapacity: Int { capacity &- _count }

  /// A Boolean value indicating whether the span is empty.
  @_alwaysEmitIntoClient
  @_transparent
  public var isEmpty: Bool { _count == 0 }

  /// A Boolean value indicating whether the span is full.
  @_alwaysEmitIntoClient
  @_transparent
  public var isFull: Bool { _count == capacity }

  /// The indices that are valid for subscripting the span, in ascending
  /// order.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public var byteOffsets: Range<Int> {
    unsafe Range(_uncheckedBounds: (0, byteCount))
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
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
  ///   - buffer: an `UnsafeMutableRawBufferPointer` to be initialized
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
      "OutputRawSpan count is not within capacity"
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
  ///   - buffer: a `Slice<UnsafeMutableRawBufferPointer>` to be initialized
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

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan {

  /// Append a single byte to this span.
  ///
  /// - Parameter value: The byte to append.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(_ value: UInt8) {
    unsafe _append(value, as: UInt8.self)
  }

  /// Remove the last byte from this span.
  ///
  /// Returns the last byte. The `OutputRawSpan` must not be empty.
  ///
  /// - Returns: The removed byte.
  @_alwaysEmitIntoClient
  @discardableResult
  @lifetime(self: copy self)
  public mutating func removeLast() -> UInt8 {
    _precondition(!isEmpty, "OutputRawSpan underflow")
    _count &-= 1
    return unsafe _tail().load(as: UInt8.self)
  }

  /// Remove the last n bytes from this span, returning the memory
  /// they occupy to the uninitialized state.
  ///
  /// `n` must not be greater than `byteCount`.
  ///
  /// - Parameter n: The number of bytes to remove.
  ///     `n` must not be negative or greater than `byteCount`.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeLast(_ n: Int) {
    _precondition(n >= 0, "Can't remove a negative number of bytes")
    _precondition(n <= _count, "OutputRawSpan underflow")
    _count &-= n
  }

  /// Remove all this span's bytes and return its memory
  /// to the uninitialized state.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeAll() {
    // TODO: Consider an option to zero the `_count` bytes being removed.
    _count = 0
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan {
  // Can we use: @_semantics("fixed_storage.check_index")
  @_alwaysEmitIntoClient @inline(__always)
  internal func _checkIndex(_ position: Int) {
    _precondition(position >= 0 && position < _count, "Index out of bounds")
  }

  /// Accesses the byte at the specified offset in the span.
  ///
  /// - Parameter byteOffset: The offset of the byte to access. `byteOffset`
  ///     must be greater than or equal to zero, and less than `byteCount`.
  @_alwaysEmitIntoClient @inline(__always)
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
  @_alwaysEmitIntoClient @inline(__always)
  @unsafe
  public subscript(unchecked byteOffset: Int) -> UInt8 {
    get {
      unsafe _start().load(fromByteOffset: byteOffset, as: UInt8.self)
    }
    set {
      unsafe _start().storeBytes(
        of: newValue, toByteOffset: byteOffset, as: UInt8.self
      )
    }
  }
}

// MARK: generic single-element append functions
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan {
  /// Appends the given value's bytes to this span's bytes.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - type: The type of the value.
  @_alwaysEmitIntoClient
  @unsafe
  @lifetime(self: copy self)
  public mutating func append<T: BitwiseCopyable>(_ value: T, as type: T.Type) {
    unsafe _append(value, as: T.self)
  }

  /// Appends the given value's bytes to this span's bytes.
  @_alwaysEmitIntoClient
  @_transparent
  @unsafe
  @_lifetime(self: copy self)
  internal mutating func _append<T>(
    _ value: T, as type: T.Type
  ) where T: BitwiseCopyable {
    _precondition(
      MemoryLayout<T>.size <= freeCapacity, "OutputRawSpan capacity overflow"
    )
    unsafe _tail().initializeMemory(as: T.self, to: value)
    _count &+= MemoryLayout<T>.size
  }

  /// Appends the given value's bytes to this span's bytes.
  ///
  /// There must be at least `MemoryLayout<T>.size` bytes available
  /// in the span.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - type: The type of the instance to store.
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append<T>(
    _ value: T, as type: T.Type
  ) where T: ConvertibleToBytes & BitwiseCopyable {
    unsafe _append(value, as: T.self)
  }

  /// Appends the given value's bytes to this span's bytes.
  ///
  /// There must be at least `MemoryLayout<T>.size` bytes available
  /// in the span.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - type: The type of the instance to store.
  ///   - byteOrder: The order in which the bytes will be encoded to the span.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 6.4, *)
  @_lifetime(self: copy self)
  public mutating func append<T>(
    _ value: T, as type: T.Type, _ byteOrder: ByteOrder
  ) where T: ConvertibleToBytes & BitwiseCopyable & FixedWidthInteger {
    let rawValue = switch byteOrder {
    case .bigEndian: value.bigEndian
    case .littleEndian: value.littleEndian
    }
    unsafe _append(rawValue, as: T.self)
  }
}

// MARK: bulk-append functions
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan {
  /// Appends the given value's bytes repeatedly to this span's bytes.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to append
  ///       to this span.
  ///   - type: The type of the instance to store repeatedly.
  @_alwaysEmitIntoClient
  @unsafe
  @lifetime(self: copy self)
  public mutating func append<T: BitwiseCopyable>(
    repeating repeatedValue: T,
    count: Int,
    as type: T.Type
  ) {
    unsafe _append(repeating: repeatedValue, count: count, as: T.self)
  }

  @_alwaysEmitIntoClient
  @unsafe
  @_lifetime(self: copy self)
  internal mutating func _append<T: BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    let total = count * MemoryLayout<T>.stride
    _precondition(total <= freeCapacity, "OutputRawSpan capacity overflow")
    unsafe _tail().initializeMemory(
      as: T.self, repeating: repeatedValue, count: count
    )
    _count &+= total
  }

  /// Appends the given value's bytes repeatedly to this span's bytes.
  ///
  /// There must be at least `count * MemoryLayout<T>.stride` bytes
  /// available in the span.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to append
  ///       to this span.
  ///   - type: The type of the instance to store repeatedly.
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append<T>(
    repeating repeatedValue: T,
    count: Int,
    as type: T.Type
  ) where T: ConvertibleToBytes & BitwiseCopyable {
    unsafe _append(repeating: repeatedValue, count: count, as: T.self)
  }

  /// Appends the given value's bytes repeatedly to this span's bytes.
  ///
  /// There must be at least `count * MemoryLayout<T>.stride` bytes
  /// available in the span.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to append
  ///       to this span.
  ///   - type: The type of the instance to store repeatedly.
  ///   - byteOrder: The order in which the bytes will be encoded to the span.
  @_alwaysEmitIntoClient
  @available(SwiftStdlib 6.4, *)
  @_lifetime(self: copy self)
  public mutating func append<T>(
    repeating repeatedValue: T,
    count: Int,
    as type: T.Type,
    _ byteOrder: ByteOrder
  ) where T: ConvertibleToBytes & BitwiseCopyable & FixedWidthInteger {
    let rawValue = switch byteOrder {
    case .bigEndian: repeatedValue.bigEndian
    case .littleEndian: repeatedValue.littleEndian
    }
    unsafe _append(repeating: rawValue, count: count, as: T.self)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan {
  /// Appends to the span as elements of a specific type.
  ///
  /// There must be at least `n * MemoryLayout<T>.stride` bytes
  /// available in the span. The address of the next uninitialized byte
  /// must be well-aligned for instances of type `type`.
  ///
  /// Inside the closure, initialize elements by appending to `typedSpan`.
  /// After the closure returns, the number of bytes initialized will be
  /// correctly updated.
  ///
  /// If the closure throws an error, the bytes for the elements appended
  /// until that point will remain initialized.
  ///
  /// - Parameters:
  ///   - n: The number of `T` elements to initialize.
  ///   - type: The type of the elements to store.
  ///   - initializer: A closure that initializes new elements.
  ///     - Parameters:
  ///       - typedSpan: An `OutputSpan` over enough bytes to initialize
  ///         the specified number of additional elements.
  @_alwaysEmitIntoClient
  @_lifetime(self: copy self)
  public mutating func append<T, E: Error>(
    upTo n: Int,
    as type: T.Type,
    initializingWith initializer:
      (_ typedSpan: inout OutputSpan<T>) throws(E) -> Void
  ) throws(E) where T: ConvertibleToBytes & BitwiseCopyable {
    let total = n * MemoryLayout<T>.stride
    _precondition(total <= freeCapacity, "OutputRawSpan capacity overflow")
    let tail = unsafe _tail()
    var initialized = 0
    defer {
      _count += initialized &* MemoryLayout<T>.stride
    }
    try unsafe tail.withMemoryRebound(to: T.self, capacity: n) { p throws(E) in
      let buffer = unsafe UnsafeMutableBufferPointer<T>(start: p, count: n)
      var typedSpan = unsafe OutputSpan<T>(buffer: buffer, initializedCount: 0)
      defer {
        initialized = unsafe typedSpan.finalize(for: buffer)
        typedSpan = .init()
      }
      try initializer(&typedSpan)
    }
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan {
  /// Borrow the underlying initialized memory for read-only access.
  @_alwaysEmitIntoClient
  @_transparent
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
  @_transparent
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

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
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
  /// 2. These initialized bytes must be located in a single contiguous
  ///     region starting at the beginning of the buffer. The rest of the buffer
  ///     must hold uninitialized memory.
  ///
  /// This function cannot verify these two invariants, and therefore
  /// this is an unsafe operation. Violating the invariants of `OutputRawSpan`
  /// may result in undefined behavior.
  ///
  /// - Parameter body: A closure that can read from and write to the
  ///     buffer and update the initialized count.
  /// - Returns: The return value of the `body` closure.
  @_alwaysEmitIntoClient
  @_transparent
  @lifetime(self: copy self)
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
    var initializedCount = _count
    defer {
      _precondition(
        0 <= initializedCount && initializedCount <= capacity,
        "OutputRawSpan capacity overflow"
      )
      _count = initializedCount
    }
    return unsafe try body(bytes, &initializedCount)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension OutputRawSpan {
  /// Consume the output span and return the number of initialized bytes.
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

  /// Consume the output span and return the number of initialized bytes.
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
