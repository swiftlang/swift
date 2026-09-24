//===--- MutableRawSpan.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if SPAN_COMPATIBILITY_STUB
import Swift
#endif

/// `MutableRawSpan` represents a contiguous region of memory
/// which contains initialized bytes.
@safe
@frozen
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
public struct MutableRawSpan: ~Copyable & ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  @usableFromInline
  internal let _count: Int

  @unsafe
  @export(implementation)
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  /// Create an empty span.
  @export(implementation)
  @inline(__always)
  @_lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    _count = 0
  }

  @unsafe
  @export(implementation)
  @_lifetime(borrow pointer)
  internal init(
    _unchecked pointer: UnsafeMutableRawPointer?,
    byteCount: Int
  ) {
    unsafe _pointer = pointer
    _count = byteCount
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan: @unchecked Sendable {}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {

  @unsafe
  @export(implementation)
  @_lifetime(borrow bytes)
  public init(
    _unsafeBytes bytes: UnsafeMutableRawBufferPointer
  ) {
    let (baseAddress, count) = (bytes.baseAddress, bytes.count)
    let span = unsafe MutableRawSpan(_unchecked: baseAddress, byteCount: count)
    self = unsafe _overrideLifetime(span, borrowing: bytes)
  }

  @unsafe
  @export(implementation)
  @_lifetime(borrow bytes)
  public init(
    _unsafeBytes bytes: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let rebased = unsafe UnsafeMutableRawBufferPointer(rebasing: bytes)
    let span = unsafe MutableRawSpan(_unsafeBytes: rebased)
    self = unsafe _overrideLifetime(span, borrowing: bytes)
  }

  @unsafe
  @export(implementation)
  @_lifetime(borrow pointer)
  public init(
    _unsafeStart pointer: UnsafeMutableRawPointer,
    byteCount: Int
  ) {
    _precondition(byteCount >= 0, "Count must not be negative")
    unsafe self.init(_unchecked: pointer, byteCount: byteCount)
  }

  @unsafe
  @export(implementation)
  @_lifetime(borrow elements)
  public init<Element: BitwiseCopyable>(
    _unsafeElements elements: UnsafeMutableBufferPointer<Element>
  ) {
    let bytes = UnsafeMutableRawBufferPointer(elements)
    let span = unsafe MutableRawSpan(_unsafeBytes: bytes)
    self = unsafe _overrideLifetime(span, borrowing: elements)
  }

  @unsafe
  @export(implementation)
  @_lifetime(borrow elements)
  public init<Element: BitwiseCopyable>(
    _unsafeElements elements: borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let rebased = unsafe UnsafeMutableBufferPointer(rebasing: elements)
    let span = unsafe MutableRawSpan(_unsafeElements: rebased)
    self = unsafe _overrideLifetime(span, borrowing: elements)
  }

  @export(implementation)
  @_lifetime(&elements)
  @unsafe
  public init<Element: BitwiseCopyable>(
    _elements elements: inout MutableSpan<Element>
  ) {
    var span = unsafe Self.init(unsafeElements: elements._reborrowed)
    span = unsafe _overrideLifetime(span, copying: ())
    self = unsafe _overrideLifetime(span, mutating: &elements)
  }

  /// Mutate the elements of a typed span as bytes.
  ///
  /// The stride of `Element` must equal its size, and the starting
  /// address of `elements` must be well-aligned for `Element`.
  ///
  /// - Parameter elements: A typed span to reinterpret as raw bytes.
  @export(implementation)
  @_lifetime(&elements)
  public init<Element: ConvertibleFromBytes & ConvertibleToBytes>(
    mutating elements: inout MutableSpan<Element>
  ) {
    self = unsafe Self.init(_elements: &elements)
  }

  /// Unsafely convert a typed span to a raw span.
  ///
  /// Creates a `MutableRawSpan` over the memory represented
  /// by a `MutableSpan<Element>`.
  ///
  /// - Parameters:
  ///   - elements: An existing `MutableSpan<Element>`, from which this
  ///     `MutableRawSpan` will inherit its lifetime.
  @export(implementation)
  @unsafe
  @_lifetime(copy elements)
  public init<Element>(
    unsafeElements elements: consuming MutableSpan<Element>
  ) {
    let (start, count) = unsafe (elements._pointer, elements._count)
    unsafe self = _overrideLifetime(
      Self.init(
        _unchecked: start,
        byteCount: (count == 1) ? MemoryLayout<Element>.size
                   : (count &* MemoryLayout<Element>.stride)
      ),
      copying: elements
    )
  }

  /// Convert a typed span to a raw span.
  ///
  /// Creates a `MutableRawSpan` over the memory represented
  /// by a `MutableSpan<Element>`.
  ///
  /// - Parameters:
  ///   - elements: An existing `MutableSpan<Element>`, from which this
  ///     `MutableRawSpan` will inherit its lifetime.
  @export(implementation)
  @_lifetime(copy elements)
  public init<Element: ConvertibleToBytes & ConvertibleFromBytes>(
    elements: consuming MutableSpan<Element>
  ) {
    self = unsafe Self.init(unsafeElements: elements)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {
  /// The number of bytes in the span.
  @export(implementation)
  @_semantics("fixed_storage.get_count")
  public var byteCount: Int { _assumeNonNegative(_count) }

  /// A Boolean value indicating whether the span is empty.
  @export(implementation)
  @_transparent
  public var isEmpty: Bool { byteCount == 0 }

  /// The valid byte offsets for accessing this span, in ascending order.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public var byteOffsets: Range<Int> {
    unsafe Range(_uncheckedBounds: (0, byteCount))
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {
  // SILOptimizer looks for fixed_storage.check_index semantics
  // for bounds checking optimizations.
  @_semantics("fixed_storage.check_index")
  @export(implementation) @inline(__always)
  internal func _checkIndex(_ position: Int) {
    _precondition(byteOffsets.contains(position), "Index out of bounds")
  }

  // SILOptimizer looks for fixed_storage.check_range semantics
  // for bounds checking optimizations.
  @_semantics("fixed_storage.check_range")
  @export(implementation) @inline(__always)
  internal func _checkRange(lowerBound: Int, upperBound: Int) {
    _precondition(
      UInt(bitPattern: lowerBound) <= _assumeNonNegative(_count) &&
      UInt(bitPattern: upperBound) <= _assumeNonNegative(_count),
      "Byte offset range out of bounds"
    )
  }

  // SILOptimizer looks for fixed_storage.check_range_offset semantics
  // for bounds checking optimizations.
  @_semantics("fixed_storage.check_range_offset")
  @export(implementation) @inline(__always)
  internal func _checkRange(offset: Int, length: Int) {
    _precondition(
      UInt(bitPattern: offset) <= _assumeNonNegative(_count) &&
      UInt(bitPattern: offset &+ length) <= _assumeNonNegative(_count),
      "Byte offset range out of bounds"
    )
  }

  /// Accesses the byte at the specified offset in the span.
  ///
  /// - Parameter byteOffset: The offset of the byte to access. `byteOffset`
  ///     must be greater than or equal to zero, and less than `byteCount`.
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
  @export(implementation) @inline(__always)
  @unsafe
  public subscript(unchecked byteOffset: Int) -> UInt8 {
    get {
      unsafe unsafeLoad(fromUncheckedByteOffset: byteOffset, as: UInt8.self)
    }
    set {
      unsafe storeBytes(
        of: newValue, toUncheckedByteOffset: byteOffset, as: UInt8.self
      )
    }
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {

  /// Calls the given closure with a pointer to the underlying bytes of
  /// the viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid only
  /// during the execution of `withUnsafeBytes(_:)`.
  /// Do not store or return the pointer for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeRawBufferPointer`
  ///   parameter that points to the viewed contiguous storage.
  ///   If `body` has a return value, that value is also
  ///   used as the return value for the `withUnsafeBytes(_:)` method.
  ///   The closure's parameter is valid only for the duration of
  ///   its execution.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_transparent
  @safe
  public func withUnsafeBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe body(.init(start: _pointer, count: _count))
  }

  /// Calls the given closure with a mutable pointer to the underlying bytes
  /// of the viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid only
  /// during the execution of `withUnsafeMutableBytes(_:)`.
  /// Do not store or return the pointer for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeMutableRawBufferPointer`
  ///   parameter that points to the viewed contiguous storage.
  ///   If `body` has a return value, that value is also
  ///   used as the return value for the `withUnsafeMutableBytes(_:)` method.
  ///   The closure's parameter is valid only for the duration of
  ///   its execution.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_transparent
  @_lifetime(self: copy self)
  @safe
  public mutating func withUnsafeMutableBytes<E: Error, Result: ~Copyable>(
    _ body: (UnsafeMutableRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe body(.init(start: _pointer, count: _count))
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension RawSpan {

  @export(implementation)
  @_lifetime(borrow mutableRawSpan)
  public init(_mutableRawSpan mutableRawSpan: borrowing MutableRawSpan) {
    let (start, count) = unsafe (mutableRawSpan._pointer, mutableRawSpan._count)
    let span = unsafe RawSpan(_unchecked: start, byteCount: count)
    self = unsafe _overrideLifetime(span, borrowing: mutableRawSpan)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {

  /// Borrow the underlying initialized memory for read-only access.
  public var bytes: RawSpan {
    @export(implementation)
    @_transparent
    @_lifetime(borrow self)
    borrowing get {
      return RawSpan(_mutableRawSpan: self)
    }
  }

  @unsafe
  @export(implementation)
  @_lifetime(borrow self)
  public borrowing func _unsafeView<T: BitwiseCopyable>(
    as type: T.Type
  ) -> Span<T> {
    let bytes = unsafe UnsafeRawBufferPointer(start: _pointer, count: _count)
    let span = unsafe Span<T>(_unsafeBytes: bytes)
    return unsafe _overrideLifetime(span, borrowing: self)
  }

  @unsafe
  @export(implementation)
  @_lifetime(&self)
  public mutating func _unsafeMutableView<T: BitwiseCopyable>(
    as type: T.Type
  ) -> MutableSpan<T> {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _pointer, count: _count
    )
    let span = unsafe MutableSpan<T>(_unsafeBytes: bytes)
    return unsafe _overrideLifetime(span, mutating: &self)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {

  /// Returns a new instance of the given type, constructed from the raw memory
  /// at the specified offset.
  ///
  /// The memory at this pointer plus `offset` must be properly aligned for
  /// accessing `T` and initialized to `T` or another type that is layout
  /// compatible with `T`.
  ///
  /// This is an unsafe operation. Failure to meet the preconditions
  /// above may produce an invalid value of `T`.
  ///
  /// - Parameters:
  ///   - offset: The offset from this pointer, in bytes. `offset` must be
  ///     nonnegative. The default is zero.
  ///   - type: The type of the instance to create.
  /// - Returns: A new instance of type `T`, read from the raw bytes at
  ///     `offset`. The returned instance is memory-managed and unassociated
  ///     with the value in the memory referenced by this pointer.
  @unsafe
  @export(implementation)
  public func unsafeLoad<T>(
    fromByteOffset offset: Int = 0, as type: T.Type
  ) -> T {
    _checkRange(offset: offset, length: MemoryLayout<T>.size)
    return unsafe unsafeLoad(fromUncheckedByteOffset: offset, as: T.self)
  }

  /// Returns a new instance of the given type, constructed from the raw memory
  /// at the specified offset.
  ///
  /// The memory at this pointer plus `offset` must be properly aligned for
  /// accessing `T` and initialized to `T` or another type that is layout
  /// compatible with `T`.
  ///
  /// This is an unsafe operation. This function does not validate the bounds
  /// of the memory access, and failure to meet the preconditions
  /// above may produce an invalid value of `T`.
  ///
  /// - Parameters:
  ///   - offset: The offset from this pointer, in bytes. `offset` must be
  ///     nonnegative. The default is zero.
  ///   - type: The type of the instance to create.
  /// - Returns: A new instance of type `T`, read from the raw bytes at
  ///     `offset`. The returned instance is memory-managed and unassociated
  ///     with the value in the memory referenced by this pointer.
  @unsafe
  @export(implementation)
  public func unsafeLoad<T>(
    fromUncheckedByteOffset offset: Int, as type: T.Type
  ) -> T {
    unsafe _start().load(fromByteOffset: offset, as: T.self)
  }

  /// Returns a new instance of the given type, constructed from the raw memory
  /// at the specified offset.
  ///
  /// The memory at this pointer plus `offset` must be initialized to `T`
  /// or another type that is layout compatible with `T`.
  ///
  /// This is an unsafe operation. Failure to meet the preconditions
  /// above may produce an invalid value of `T`.
  ///
  /// - Parameters:
  ///   - offset: The offset from this pointer, in bytes. `offset` must be
  ///     nonnegative. The default is zero.
  ///   - type: The type of the instance to create.
  /// - Returns: A new instance of type `T`, read from the raw bytes at
  ///     `offset`. The returned instance isn't associated
  ///     with the value in the range of memory referenced by this pointer.
  @unsafe
  @export(implementation)
  public func unsafeLoadUnaligned<T: BitwiseCopyable>(
    fromByteOffset offset: Int = 0, as type: T.Type
  ) -> T {
    _checkRange(offset: offset, length: MemoryLayout<T>.size)
    return unsafe unsafeLoadUnaligned(fromUncheckedByteOffset: offset, as: T.self)
  }

  /// Returns a new instance of the given type, constructed from the raw memory
  /// at the specified offset.
  ///
  /// The memory at this pointer plus `offset` must be initialized to `T`
  /// or another type that is layout compatible with `T`.
  ///
  /// This is an unsafe operation. This function does not validate the bounds
  /// of the memory access, and failure to meet the preconditions
  /// above may produce an invalid value of `T`.
  ///
  /// - Parameters:
  ///   - offset: The offset from this pointer, in bytes. `offset` must be
  ///     nonnegative. The default is zero.
  ///   - type: The type of the instance to create.
  /// - Returns: A new instance of type `T`, read from the raw bytes at
  ///     `offset`. The returned instance isn't associated
  ///     with the value in the range of memory referenced by this pointer.
  @unsafe
  @export(implementation)
  public func unsafeLoadUnaligned<T: BitwiseCopyable>(
    fromUncheckedByteOffset offset: Int, as type: T.Type
  ) -> T {
    unsafe _start().loadUnaligned(fromByteOffset: offset, as: T.self)
  }

  /// Returns a value constructed from the raw memory at the specified offset.
  ///
  /// The range of bytes required to construct a value of type `T` starting at
  /// `offset` must be completely within the span.
  /// `offset` is not required to be aligned for `T`.
  ///
  /// - Parameters:
  ///   - offset: The offset from the beginning of this span, in bytes.
  ///     `offset` must be nonnegative.
  ///   - type: The type of the instance to create.
  /// - Returns: A new value of type `T`, read from `offset`.
  @export(implementation)
  public func load<T: ConvertibleFromBytes>(
    fromByteOffset offset: Int,
    as type: T.Type
  ) -> T {
    unsafe unsafeLoadUnaligned(fromByteOffset: offset, as: T.self)
  }

  /// Returns a value constructed from the raw memory at the specified offset.
  ///
  /// The range of bytes required to construct a value of type `T` starting at
  /// `offset` must be completely within the span.
  /// `offset` is not required to be aligned for `T`.
  ///
  /// - Parameters:
  ///   - offset: The offset from the beginning of this span, in bytes.
  ///     `offset` must be nonnegative.
  ///   - type: The type of the instance to create.
  ///   - byteOrder: The order in which the bytes will be decoded.
  /// - Returns: A new value of type `T`, read from `offset`.
  @export(implementation)
  @available(StdlibDeploymentTarget 6.4, *)
  public func load<T: ConvertibleFromBytes & FixedWidthInteger>(
    fromByteOffset offset: Int,
    as type: T.Type,
    _ byteOrder: ByteOrder
  ) -> T {
    let rawValue = load(fromByteOffset: offset, as: T.self)
    return switch byteOrder {
    case .bigEndian: rawValue.bigEndian
    case .littleEndian: rawValue.littleEndian
    }
  }

  /// Stores the given value's bytes into the span's raw memory at the
  /// specified byte offset.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - offset: The offset from the start of the span, in bytes.
  ///     `offset` must be nonnegative. The default is zero.
  ///   - type: The type of `value`.
  @unsafe
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func storeBytes<T: BitwiseCopyable>(
    of value: T, toByteOffset offset: Int = 0, as type: T.Type
  ) {
    unsafe _storeBytes(of: value, toByteOffset: offset, as: T.self)
  }

  @unsafe
  @export(implementation) @_transparent
  @_lifetime(self: copy self)
  internal mutating func _storeBytes<T: BitwiseCopyable>(
    of value: T, toByteOffset offset: Int, as type: T.Type
  ) {
    _checkRange(offset: offset, length: MemoryLayout<T>.size)
    unsafe storeBytes(of: value, toUncheckedByteOffset: offset, as: T.self)
  }

  /// Stores the given value's bytes into the span's raw memory at the
  /// specified byte offset.
  ///
  /// This function does not validate `offset`; this is an unsafe operation.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - offset: The offset from the start of the span, in bytes.
  ///     `offset` must be nonnegative.
  ///   - type: The type of `value`.
  @unsafe
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func storeBytes<T: BitwiseCopyable>(
    of value: T, toUncheckedByteOffset offset: Int, as type: T.Type
  ) {
    unsafe _start().storeBytes(of: value, toByteOffset: offset, as: T.self)
  }

  /// Stores the given value's bytes to the specified offset into
  /// the span's memory.
  ///
  /// The range of bytes required to store a value of type `T` starting at
  /// byte offset `offset` must be completely within the span.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - offset: The offset in bytes into the span's memory at which to begin
  ///       writing the bytes from the value.
  ///   - type: The type of the instance to store.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func storeBytes<T: ConvertibleToBytes & BitwiseCopyable>(
    of value: T, toByteOffset offset: Int, as type: T.Type
  ) {
    unsafe _storeBytes(of: value, toByteOffset: offset, as: T.self)
  }

  /// Stores the given value's bytes to the specified offset into
  /// the span's memory.
  ///
  /// The range of bytes required to store a value of type `T` starting at
  /// byte offset `offset` must be completely within the span.
  /// `offset` is not required to be aligned for `T`.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - offset: The offset in bytes into the span's memory at which to begin
  ///       writing the bytes from the value.
  ///   - type: The type of the instance to store.
  ///   - byteOrder: The order in which the bytes will be encoded to the span.
  @export(implementation)
  @available(StdlibDeploymentTarget 6.4, *)
  @_lifetime(self: copy self)
  public mutating func storeBytes<
    T: ConvertibleToBytes & BitwiseCopyable & FixedWidthInteger
  >(
    of value: T,
    toByteOffset offset: Int,
    as type: T.Type,
    _ byteOrder: ByteOrder
  ) {
    switch byteOrder {
    case .bigEndian:
      storeBytes(of: value.bigEndian, toByteOffset: offset, as: T.self)
    case .littleEndian:
      storeBytes(of: value.littleEndian, toByteOffset: offset, as: T.self)
    }
  }

  /// Stores the given value's bytes repeatedly into this span's memory.
  ///
  /// There must be at least `count * MemoryLayout<T>.stride` bytes
  /// available in the span.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to store
  ///      into this span.
  ///   - type: The type of the instance to store repeatedly.
  @unsafe
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func storeBytes<T: BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    unsafe _storeBytes(repeating: repeatedValue, count: count, as: T.self)
  }

  @unsafe
  @export(implementation) @_transparent
  @_lifetime(self: copy self)
  internal mutating func _storeBytes<T: BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    _precondition(
      count * MemoryLayout<T>.stride <= _count,
      "Span cannot contain every element"
    )
    unsafe _start().withMemoryRebound(to: T.self, capacity: count) {
      unsafe $0.update(repeating: repeatedValue, count: count)
    }
  }

  /// Stores the given value's bytes repeatedly into this span's memory.
  ///
  /// There must be at least `count * MemoryLayout<T>.stride` bytes
  /// available in the span.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to store
  ///      into this span.
  ///   - type: The type of the instance to store repeatedly.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func storeBytes<T: ConvertibleToBytes & BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    unsafe _storeBytes(repeating: repeatedValue, count: count, as: T.self)
  }

  /// Stores the given value's bytes repeatedly into this span's memory.
  ///
  /// There must be at least `count * MemoryLayout<T>.stride` bytes
  /// available in the span.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to store
  ///      into this span.
  ///   - type: The type of the instance to store repeatedly.
  ///   - byteOrder: The order in which the bytes will be encoded to the span.
  @export(implementation)
  @available(StdlibDeploymentTarget 6.4, *)
  @_lifetime(self: copy self)
  public mutating func storeBytes<
    T: ConvertibleToBytes & BitwiseCopyable & FixedWidthInteger
  >(
    repeating repeatedValue: T,
    count: Int,
    as type: T.Type,
    _ byteOrder: ByteOrder
  ) {
    let value = switch byteOrder {
    case .bigEndian: repeatedValue.bigEndian
    case .littleEndian: repeatedValue.littleEndian
    }
    storeBytes(repeating: value, count: count, as: T.self)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {

  /// Updates every byte of this span to the given value.
  ///
  /// - Parameter repeatedByte: The value to set for every byte.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateAll(repeating repeatedByte: UInt8) {
    guard !isEmpty else { return }
    unsafe _start().withMemoryRebound(to: UInt8.self, capacity: byteCount) {
      unsafe $0.update(repeating: repeatedByte, count: byteCount)
    }
  }

  /// Updates every byte within the supplied range of positions
  /// to the given value.
  ///
  /// - Parameters:
  ///   - subrange: A valid range of positions. Every position in this range
  ///      must be within the bounds of this `MutableRawSpan`.
  ///   - repeatedByte: The value to set for every byte in `subrange`.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: Range<Int>,
    repeating repeatedByte: UInt8,
  ) {
    var span = self._mutatingExtracting(subrange)
    span.updateAll(repeating: repeatedByte)
  }

  /// Updates every byte within the supplied range of positions
  /// to the given value.
  ///
  /// - Parameters:
  ///   - subrange: A valid range of positions. Every position in this range
  ///      must be within the bounds of this `MutableRawSpan`.
  ///   - repeatedByte: The value to set for every byte in `subrange`.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: some RangeExpression<Int>,
    repeating repeatedByte: UInt8,
  ) {
    updateSubrange(subrange.relative(to: byteOffsets), repeating: repeatedByte)
  }

  /// Updates every byte of this span to the given value.
  ///
  /// - Parameters:
  ///   - subrange: An unbounded range, selecting every position of this span.
  ///   - repeatedByte: The value to set for every byte.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: UnboundedRange,
    repeating repeatedByte: UInt8,
  ) {
    updateAll(repeating: repeatedByte)
  }

  /// Copies bytes from source into this span.
  ///
  /// `source` must have exactly as many bytes as this span.
  ///
  /// - Parameter source: The bytes to copy into this span.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateAll(copying source: RawSpan) {
    precondition(source.byteCount == self.byteCount)
    if self.isEmpty { return }
    unsafe _start().copyMemory(from: source._start(), byteCount: byteCount)
  }

  /// Copies bytes from source into the supplied range of positions
  /// within this span.
  ///
  /// `source` must have exactly as many bytes as `subrange`.
  ///
  /// - Parameters:
  ///   - subrange: A valid range of positions. Every position in this range
  ///      must be within the bounds of this `MutableRawSpan`.
  ///   - source: The bytes to copy into `subrange`.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: Range<Int>, copying source: RawSpan
  ) {
    var span = self._mutatingExtracting(subrange)
    span.updateAll(copying: source)
  }

  /// Copies bytes from source into the supplied range of positions
  /// within this span.
  ///
  /// `source` must have exactly as many bytes as `subrange`.
  ///
  /// - Parameters:
  ///   - subrange: A valid range of positions. Every position in this range
  ///      must be within the bounds of this `MutableRawSpan`.
  ///   - source: The bytes to copy into `subrange`.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: some RangeExpression<Int>, copying source: RawSpan
  ) {
    updateSubrange(subrange.relative(to: byteOffsets), copying: source)
  }

  /// Copies bytes from source into this span.
  ///
  /// `source` must have exactly as many bytes as this span.
  ///
  /// - Parameters:
  ///   - subrange: An unbounded range, selecting every position of this span.
  ///   - source: The bytes to copy into this span.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: UnboundedRange, copying source: RawSpan
  ) {
    updateAll(copying: source)
  }

  /// Moves bytes from source into this span, leaving the source empty.
  ///
  /// `source` must have exactly as many initialized bytes as this span.
  /// When this function returns, `source` is empty, and its memory has been
  /// returned to the uninitialized state.
  ///
  /// - Parameter source: The bytes to move into this span.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateAll(moving source: inout OutputRawSpan) {
    precondition(source.byteCount == self.byteCount)
    if self.isEmpty { return }
    unsafe _start().copyMemory(from: source._start(), byteCount: byteCount)
    source.removeAll()
  }

  /// Moves bytes from source into the supplied range of positions
  /// within this span, leaving the source empty.
  ///
  /// `source` must have exactly as many initialized bytes as `subrange`.
  /// When this function returns, `source` is empty, and its memory has been
  /// returned to the uninitialized state.
  ///
  /// - Parameters:
  ///   - subrange: A valid range of positions. Every position in this range
  ///      must be within the bounds of this `MutableRawSpan`.
  ///   - source: The bytes to move into `subrange`.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: Range<Int>, moving source: inout OutputRawSpan
  ) {
    var span = self._mutatingExtracting(subrange)
    span.updateAll(moving: &source)
  }

  /// Moves bytes from source into the supplied range of positions
  /// within this span, leaving the source empty.
  ///
  /// `source` must have exactly as many initialized bytes as `subrange`.
  /// When this function returns, `source` is empty, and its memory has been
  /// returned to the uninitialized state.
  ///
  /// - Parameters:
  ///   - subrange: A valid range of positions. Every position in this range
  ///      must be within the bounds of this `MutableRawSpan`.
  ///   - source: The bytes to move into `subrange`.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: some RangeExpression<Int>, moving source: inout OutputRawSpan
  ) {
    updateSubrange(subrange.relative(to: byteOffsets), moving: &source)
  }

  /// Moves bytes from source into this span, leaving the source empty.
  ///
  /// `source` must have exactly as many initialized bytes as this span.
  /// When this function returns, `source` is empty, and its memory has been
  /// returned to the uninitialized state.
  ///
  /// - Parameters:
  ///   - subrange: An unbounded range, selecting every position of this span.
  ///   - source: The bytes to move into this span.
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func updateSubrange(
    _ subrange: UnboundedRange, moving source: inout OutputRawSpan
  ) {
    updateAll(moving: &source)
  }

#if !SPAN_COMPATIBILITY_STUB
  /// Copies every byte of the source into this span, starting at byteOffset.
  ///
  /// This span must have enough space between `byteOffset` and its end for
  /// every byte `source` provides.
  ///
  /// When the function returns, the value of `byteOffset` is the offset after
  /// the last written byte in the span.
  ///
  /// If reading from `source` throws an error, the bytes copied before
  /// the error occurred remain in this span, and `byteOffset` is updated to
  /// the offset after the last written byte.
  ///
  /// - Parameters:
  ///   - byteOffset: The offset at which to start copying. It must be a valid
  ///      offset into this span, or its `byteCount`. On return, it is the
  ///      offset after the last byte written.
  ///   - source: The bytes to copy into this span.
  /// - Throws: Any error thrown while reading from `source`.
  @export(implementation)
  @available(SwiftStdlib 6.4, *)
  @_lifetime(self: copy self)
  public mutating func updateElements<
    I: Iterable & ~Escapable & ~Copyable
  >(
    from byteOffset: inout Int, copying source: borrowing I
  ) throws(I.Failure) where I.Element == UInt8 {
    var iterator = source.makeBorrowingIterator()
    try updateElements(from: &byteOffset, copying: &iterator)
    let next = try iterator.nextSpan()
    _precondition(next.isEmpty)
  }

  /// Copies every byte of the source into this span, starting at byteOffset.
  ///
  /// This span must have space between `byteOffset` and its end for every byte
  /// `source` provides.
  ///
  /// - Parameters:
  ///   - byteOffset: The offset at which to start copying. It must be a valid
  ///      offset into this span, or its `byteCount`.
  ///   - source: The bytes to copy into this span.
  /// - Returns: The offset after the last byte written.
  @export(implementation)
  @available(SwiftStdlib 6.4, *)
  @_lifetime(self: copy self)
  public mutating func updateElements<
    I: Iterable & ~Escapable & ~Copyable
  >(
    from byteOffset: Int, copying source: borrowing I
  ) -> Int where I.Element == UInt8, I.Failure == Never {
    var byteOffset = byteOffset
    updateElements(from: &byteOffset, copying: source)
    return byteOffset
  }

  /// Copies bytes from an iterator into this span, starting at byteOffset.
  ///
  /// Copying stops as soon as `source` is exhausted, or the end of this span
  /// is reached, whichever comes first.
  ///
  /// When the function returns, the value of `byteOffset` is the offset after
  /// the last written byte in the span.
  ///
  /// If reading from `source` throws an error, the bytes copied before
  /// the error occurred remain in this span, and `byteOffset` is updated to
  /// the offset after the last written byte.
  ///
  /// - Parameters:
  ///   - byteOffset: The offset at which to start copying. It must be a valid
  ///      offset into this span, or its `byteCount`. On return, it is the
  ///      offset after the last byte written.
  ///   - source: An iterator over the bytes to copy into this span. On
  ///      return, it is positioned after the last byte copied.
  /// - Throws: Any error thrown while reading from `source`.
  @export(implementation)
  @available(SwiftStdlib 6.4, *)
  @_lifetime(self: copy self)
  public mutating func updateElements<
    I: BorrowingIteratorProtocol & ~Escapable & ~Copyable
  >(
    from byteOffset: inout Int,
    copying source: inout I
  ) throws(I.Failure) where I.Element == UInt8 {
    _precondition(
      UInt(bitPattern: byteOffset) <= UInt(bitPattern: _count),
      "Byte offset out of bounds"
    )
    while byteOffset < byteCount {
      let bytes = try source.nextSpan(maxCount: byteCount &- byteOffset)
      if bytes.isEmpty { break }
      updateSubrange(
        byteOffset ..< (byteOffset &+ bytes.count),
        copying: RawSpan(elements: bytes)
      )
      byteOffset &+= bytes.count
    }
  }
#endif // !SPAN_COMPATIBILITY_STUB
}

// MARK: sub-spans
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(_ bounds: Range<Int>) -> Self {
    _checkRange(lowerBound: bounds.lowerBound, upperBound: bounds.upperBound)
    return unsafe _mutatingExtracting(unchecked: bounds)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @available(*, deprecated, renamed: "_mutatingExtracting(_:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(_ bounds: Range<Int>) -> Self {
    _mutatingExtracting(bounds)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(_ bounds: Range<Int>) -> Self {
    _checkRange(lowerBound: bounds.lowerBound, upperBound: bounds.upperBound)
    return unsafe _consumingExtracting(unchecked: bounds)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(unchecked bounds: Range<Int>) -> Self {
    let newStart = unsafe _pointer?.advanced(by: bounds.lowerBound)
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: bounds.count)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @available(*, deprecated, renamed: "_mutatingExtracting(unchecked:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(unchecked bounds: Range<Int>) -> Self {
    unsafe _mutatingExtracting(unchecked: bounds)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(unchecked bounds: Range<Int>) -> Self {
    let newStart = unsafe _pointer?.advanced(by: bounds.lowerBound)
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: bounds.count)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(
    _ bounds: some RangeExpression<Int>
  ) -> Self {
    _mutatingExtracting(bounds.relative(to: byteOffsets))
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @available(*, deprecated, renamed: "_mutatingExtracting(_:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(
    _ bounds: some RangeExpression<Int>
  ) -> Self {
    _mutatingExtracting(bounds)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(
    _ bounds: some RangeExpression<Int>
  ) -> Self {
    _consumingExtracting(bounds.relative(to: byteOffsets))
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(
    unchecked bounds: ClosedRange<Int>
  ) -> Self {
    let range = unsafe Range(
      _uncheckedBounds: (bounds.lowerBound, bounds.upperBound + 1)
    )
    return unsafe _mutatingExtracting(unchecked: range)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @available(*, deprecated, renamed: "_mutatingExtracting(unchecked:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(
    unchecked bounds: ClosedRange<Int>
  ) -> Self {
    unsafe _mutatingExtracting(unchecked: bounds)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///   this range must be within the bounds of this `MutableRawSpan`.
  /// - Returns: A `MutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(
    unchecked bounds: ClosedRange<Int>
  ) -> Self {
    let range = unsafe Range(
      _uncheckedBounds: (bounds.lowerBound, bounds.upperBound + 1)
    )
    return unsafe _consumingExtracting(unchecked: range)
  }

  /// Constructs a new span over all the bytes of this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Returns: A `MutableRawSpan` over all the bytes of this span.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(_: UnboundedRange) -> Self {
    unsafe _overrideLifetime(
      Self(_unchecked: _pointer, byteCount: _count), mutating: &self
    )
  }

  @export(implementation) @inline(__always)
  internal var _reborrowed: Self {
    @_lifetime(&self)
    mutating get { _mutatingExtracting(...) }
  }

  /// Constructs a new span over all the bytes of this span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Returns: A `MutableRawSpan` over all the bytes of this span.
  ///
  /// - Complexity: O(1)
  @available(*, deprecated, renamed: "_mutatingExtracting(_:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(_: UnboundedRange) -> Self {
    _mutatingExtracting(...)
  }

  /// Constructs a new span over all the bytes of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Returns: A `MutableRawSpan` over all the bytes of this span.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(_: UnboundedRange) -> Self {
    self
  }
}

// MARK: prefixes and suffixes
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableRawSpan {

  /// Returns a span containing the initial bytes of this span,
  /// up to the specified maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the bytes.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(first maxLength: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, byteCount)
    let newSpan = unsafe Self(_unchecked: _pointer, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  /// Returns a span containing the initial bytes of this span,
  /// up to the specified maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the bytes.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @available(*, deprecated, renamed: "_mutatingExtracting(first:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(first maxLength: Int) -> Self {
    _mutatingExtracting(first: maxLength)
  }

  /// Returns a span containing the initial bytes of this span,
  /// up to the specified maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the bytes.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(first maxLength: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, byteCount)
    let newSpan = unsafe Self(_unchecked: _pointer, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  /// Returns a span over all but the specified number of trailing bytes.
  ///
  /// If the number of bytes to drop exceeds the number of bytes in
  /// the span, the result is an empty span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// - Parameter k: The number of bytes to drop off the end of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span leaving off the specified number of trailing bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(droppingLast k: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(k >= 0, "Can't drop a negative number of bytes")
    let droppedCount = min(k, byteCount)
    let newCount = byteCount &- droppedCount
    let newSpan = unsafe Self(_unchecked: _pointer, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  /// Returns a span over all but the specified number of trailing bytes.
  ///
  /// If the number of bytes to drop exceeds the number of bytes in
  /// the span, the result is an empty span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// - Parameter k: The number of bytes to drop off the end of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span leaving off the specified number of trailing bytes.
  ///
  /// - Complexity: O(1)
  @available(*, deprecated, renamed: "_mutatingExtracting(droppingLast:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(droppingLast k: Int) -> Self {
    _mutatingExtracting(droppingLast: k)
  }

  /// Returns a span over all but the specified number of trailing bytes.
  ///
  /// If the number of bytes to drop exceeds the number of bytes in
  /// the span, the result is an empty span.
  ///
  /// - Parameter k: The number of bytes to drop off the end of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span leaving off the specified number of trailing bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(droppingLast k: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(k >= 0, "Can't drop a negative number of bytes")
    let droppedCount = min(k, byteCount)
    let newCount = byteCount &- droppedCount
    let newSpan = unsafe Self(_unchecked: _pointer, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  /// Returns a span containing the trailing bytes of this span,
  /// up to the specified maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the bytes.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(last maxLength: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, byteCount)
    let newStart = unsafe _pointer?.advanced(by: byteCount &- newCount)
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  /// Returns a span containing the trailing bytes of this span,
  /// up to the specified maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the bytes.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @available(*, deprecated, renamed: "_mutatingExtracting(last:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(last maxLength: Int) -> Self {
    _mutatingExtracting(last: maxLength)
  }

  /// Returns a span containing the trailing bytes of this span,
  /// up to the specified maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the bytes.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(last maxLength: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, byteCount)
    let newStart = unsafe _pointer?.advanced(by: byteCount &- newCount)
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  /// Returns a span over all but the specified number of initial bytes.
  ///
  /// If the number of bytes to drop exceeds the number of bytes in
  /// the span, the result is an empty span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter k: The number of bytes to drop from the beginning of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span starting after the specified number of bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(&self)
  mutating public func _mutatingExtracting(droppingFirst k: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(k >= 0, "Can't drop a negative number of bytes")
    let droppedCount = min(k, byteCount)
    let newStart = unsafe _pointer?.advanced(by: droppedCount)
    let newCount = byteCount &- droppedCount
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  /// Returns a span over all but the specified number of initial bytes.
  ///
  /// If the number of bytes to drop exceeds the number of bytes in
  /// the span, the result is an empty span.
  ///
  /// The returned span represents a mutation of this span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter k: The number of bytes to drop from the beginning of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span starting after the specified number of bytes.
  ///
  /// - Complexity: O(1)
  @available(*, deprecated, renamed: "_mutatingExtracting(droppingFirst:)")
  @export(implementation)
  @_lifetime(&self)
  mutating public func extracting(droppingFirst k: Int) -> Self {
    _mutatingExtracting(droppingFirst: k)
  }

  /// Returns a span over all but the specified number of initial bytes.
  ///
  /// If the number of bytes to drop exceeds the number of bytes in
  /// the span, the result is an empty span.
  ///
  /// The returned span's first byte is always at offset 0. Extracted spans
  /// do not share their indices with the span from which they are extracted.
  ///
  /// - Parameter k: The number of bytes to drop from the beginning of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span starting after the specified number of bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  consuming public func _consumingExtracting(droppingFirst k: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(k >= 0, "Can't drop a negative number of bytes")
    let droppedCount = min(k, byteCount)
    let newStart = unsafe _pointer?.advanced(by: droppedCount)
    let newCount = byteCount &- droppedCount
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
#else
    fatalError("Unsupported compiler")
#endif
  }
}

#if !SPAN_COMPATIBILITY_STUB
@available(SwiftStdlib 6.4, *)
extension MutableRawSpan: Iterable {
  @available(SwiftStdlib 6.4, *)
  public typealias Failure = Never

  @export(implementation)
  public var underestimatedCount: Int {
    self.byteCount
  }

  @available(SwiftStdlib 6.4, *)
  @export(implementation)
  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<UInt8>.BorrowingIterator {
    .init(Span(viewing: self.bytes))
  }
}
#endif
