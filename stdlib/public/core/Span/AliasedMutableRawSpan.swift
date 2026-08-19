//===--- AliasedMutableRawSpan.swift --------------------------------------===//
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

/// `AliasedMutableRawSpan` represents a contiguous region of memory which
/// contains initialized bytes that can be both read and written, and which may
/// be aliased by other references to the same memory.
///
/// `AliasedMutableRawSpan` is to `MutableRawSpan` what `AliasedMutableSpan` is
/// to `MutableSpan`. Because it already accounts for the presence of aliases,
/// it is `Copyable`, and its mutating operations are non-mutating on `self`:
/// storing bytes changes the referenced memory, not the span.
@frozen
@safe
@available(SwiftStdlib 6.5, *)
public struct AliasedMutableRawSpan: ~Escapable, Copyable, BitwiseCopyable {

  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  @usableFromInline
  internal let _count: Int

  @unsafe
  @export(implementation)
  @inline(__always)
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
  @inline(__always)
  @_lifetime(borrow pointer)
  internal init(
    _unchecked pointer: UnsafeMutableRawPointer?,
    byteCount: Int
  ) {
    unsafe _pointer = pointer
    _count = byteCount
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan: @unchecked Sendable {}

// MARK: - unsafe construction

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// Unsafely create an `AliasedMutableRawSpan` over initialized memory.
  ///
  /// The memory in `bytes` must remain valid and initialized throughout the
  /// lifetime of the newly-created span. Unlike `MutableRawSpan`, the memory
  /// is *not* required to be exclusively accessed.
  ///
  /// - Parameters:
  ///   - bytes: an `UnsafeMutableRawBufferPointer` to initialized memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow bytes)
  public init(_unsafeBytes bytes: UnsafeMutableRawBufferPointer) {
    let (baseAddress, count) = (bytes.baseAddress, bytes.count)
    let span = unsafe AliasedMutableRawSpan(
      _unchecked: baseAddress, byteCount: count
    )
    self = unsafe _overrideLifetime(span, borrowing: bytes)
  }

  /// Unsafely create an `AliasedMutableRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - bytes: a `Slice<UnsafeMutableRawBufferPointer>` to initialized
  ///     memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow bytes)
  public init(
    _unsafeBytes bytes: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let rebased = unsafe UnsafeMutableRawBufferPointer(rebasing: bytes)
    let span = unsafe AliasedMutableRawSpan(_unsafeBytes: rebased)
    self = unsafe _overrideLifetime(span, borrowing: bytes)
  }

  /// Unsafely create an `AliasedMutableRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized byte.
  ///   - byteCount: the number of initialized bytes in the span.
  @unsafe
  @export(implementation)
  @_lifetime(borrow pointer)
  public init(_unsafeStart pointer: UnsafeMutableRawPointer, byteCount: Int) {
    _precondition(byteCount >= 0, "Count must not be negative")
    unsafe self.init(_unchecked: pointer, byteCount: byteCount)
  }

  /// Unsafely create an `AliasedMutableRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - elements: an `UnsafeMutableBufferPointer<Element>` to initialized
  ///     memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow elements)
  public init<Element: BitwiseCopyable>(
    _unsafeElements elements: UnsafeMutableBufferPointer<Element>
  ) {
    let bytes = UnsafeMutableRawBufferPointer(elements)
    let span = unsafe AliasedMutableRawSpan(_unsafeBytes: bytes)
    self = unsafe _overrideLifetime(span, borrowing: elements)
  }

  /// Unsafely create an `AliasedMutableRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - elements: a `Slice<UnsafeMutableBufferPointer<Element>>` to
  ///     initialized memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow elements)
  public init<Element: BitwiseCopyable>(
    _unsafeElements elements:
      borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let rebased = unsafe UnsafeMutableBufferPointer(rebasing: elements)
    let span = unsafe AliasedMutableRawSpan(_unsafeElements: rebased)
    self = unsafe _overrideLifetime(span, borrowing: elements)
  }
}

// MARK: - conversion from typed aliased spans

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// Unsafely convert a typed aliased mutable span to a raw aliased mutable
  /// span.
  ///
  /// This is unsafe because `Element` may contain padding bytes, which are
  /// not necessarily initialized, and because writing arbitrary bytes may
  /// produce an invalid instance of `Element`.
  ///
  /// - Parameters:
  ///   - elements: An existing `AliasedMutableSpan<Element>`, from which this
  ///     span will inherit its lifetime.
  @unsafe
  @export(implementation)
  @_lifetime(copy elements)
  public init<Element>(unsafeElements elements: AliasedMutableSpan<Element>) {
    let (start, count) = unsafe (elements._pointer, elements._count)
    let span = unsafe AliasedMutableRawSpan(
      _unchecked: start,
      byteCount: (count == 1) ? MemoryLayout<Element>.size
                 : (count &* MemoryLayout<Element>.stride)
    )
    self = unsafe _overrideLifetime(span, copying: elements)
  }

  /// Convert a typed aliased mutable span to a raw aliased mutable span.
  ///
  /// - Parameters:
  ///   - elements: An existing `AliasedMutableSpan<Element>`, from which this
  ///     span will inherit its lifetime.
  @export(implementation)
  @_lifetime(copy elements)
  public init<Element: ConvertibleFromBytes & ConvertibleToBytes>(
    elements: AliasedMutableSpan<Element>
  ) {
    self = unsafe Self.init(unsafeElements: elements)
  }
}

// MARK: - basic properties

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// The number of bytes in the span.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_semantics("fixed_storage.get_count")
  public var byteCount: Int { _assumeNonNegative(_count) }

  /// A Boolean value indicating whether the span is empty.
  ///
  /// - Complexity: O(1)
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

// MARK: - byte access

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {
  // SILOptimizer looks for fixed_storage.check_index semantics
  // for bounds checking optimizations.
  @_semantics("fixed_storage.check_index")
  @export(implementation) @inline(__always)
  internal func _checkIndex(_ position: Int) {
    _precondition(byteOffsets.contains(position), "Index out of bounds")
  }

  /// Accesses the byte at the specified offset in the span.
  ///
  /// The setter is non-mutating because storing a byte does not change the
  /// span itself, only the contents of the memory it references.
  ///
  /// - Parameter byteOffset: The offset of the byte to access. `byteOffset`
  ///     must be greater than or equal to zero, and less than `byteCount`.
  @export(implementation) @inline(__always)
  public subscript(_ byteOffset: Int) -> UInt8 {
    get {
      _checkIndex(byteOffset)
      return unsafe self[unchecked: byteOffset]
    }
    nonmutating set {
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
    nonmutating set {
      unsafe storeBytes(
        of: newValue, toUncheckedByteOffset: byteOffset, as: UInt8.self
      )
    }
  }
}

// MARK: - UnsafeRawBufferPointer access hatch

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// Calls the given closure with a pointer to the underlying bytes of
  /// the viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid only
  /// during the execution of `withUnsafeBytes(_:)`.
  /// Do not store or return the pointer for later use.
  ///
  /// - Note: Because the storage may be aliased, its contents may change
  ///   while `body` is executing.
  ///
  /// - Parameter body: A closure with an `UnsafeRawBufferPointer`
  ///   parameter that points to the viewed contiguous storage.
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
  /// - Note: Because the storage may be aliased, its contents may change
  ///   while `body` is executing.
  ///
  /// - Parameter body: A closure with an `UnsafeMutableRawBufferPointer`
  ///   parameter that points to the viewed contiguous storage.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_transparent
  @safe
  public func withUnsafeMutableBytes<E: Error, Result: ~Copyable>(
    _ body: (UnsafeMutableRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe body(.init(start: _pointer, count: _count))
  }
}

// MARK: - load

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// Returns a new instance of the given type, constructed from the raw
  /// memory at the specified offset.
  ///
  /// This is an unsafe operation. Failure to meet the preconditions of
  /// alignment and layout compatibility may produce an invalid value of `T`.
  ///
  /// - Parameters:
  ///   - offset: The offset from the beginning of this span, in bytes.
  ///     `offset` must be nonnegative. The default is zero.
  ///   - type: The type of the instance to create.
  /// - Returns: A new instance of type `T`, read from the raw bytes at
  ///     `offset`.
  @unsafe
  @export(implementation)
  public func unsafeLoad<T>(
    fromByteOffset offset: Int = 0, as type: T.Type
  ) -> T {
    _precondition(
      UInt(bitPattern: offset) <= UInt(bitPattern: _count) &&
      MemoryLayout<T>.size <= (_count &- offset),
      "Byte offset range out of bounds"
    )
    return unsafe unsafeLoad(fromUncheckedByteOffset: offset, as: T.self)
  }

  /// Returns a new instance of the given type, constructed from the raw
  /// memory at the specified offset.
  ///
  /// This is an unsafe operation. This function does not validate the bounds
  /// of the memory access.
  ///
  /// - Parameters:
  ///   - offset: The offset from the beginning of this span, in bytes.
  ///     `offset` must be nonnegative.
  ///   - type: The type of the instance to create.
  /// - Returns: A new instance of type `T`, read from the raw bytes at
  ///     `offset`.
  @unsafe
  @export(implementation)
  public func unsafeLoad<T>(
    fromUncheckedByteOffset offset: Int, as type: T.Type
  ) -> T {
    unsafe _start().load(fromByteOffset: offset, as: T.self)
  }

  /// Returns a new instance of the given type, constructed from the raw
  /// memory at the specified offset.
  ///
  /// This is an unsafe operation. Failure to meet the precondition of layout
  /// compatibility may produce an invalid value of `T`.
  ///
  /// - Parameters:
  ///   - offset: The offset from the beginning of this span, in bytes.
  ///     `offset` must be nonnegative. The default is zero.
  ///   - type: The type of the instance to create.
  /// - Returns: A new instance of type `T`, read from the raw bytes at
  ///     `offset`.
  @unsafe
  @export(implementation)
  public func unsafeLoadUnaligned<T: BitwiseCopyable>(
    fromByteOffset offset: Int = 0, as type: T.Type
  ) -> T {
    _precondition(
      UInt(bitPattern: offset) <= UInt(bitPattern: _count) &&
      MemoryLayout<T>.size <= (_count &- offset),
      "Byte offset range out of bounds"
    )
    return unsafe unsafeLoadUnaligned(
      fromUncheckedByteOffset: offset, as: T.self
    )
  }

  /// Returns a new instance of the given type, constructed from the raw
  /// memory at the specified offset.
  ///
  /// This is an unsafe operation. This function does not validate the bounds
  /// of the memory access.
  ///
  /// - Parameters:
  ///   - offset: The offset from the beginning of this span, in bytes.
  ///     `offset` must be nonnegative.
  ///   - type: The type of the instance to create.
  /// - Returns: A new instance of type `T`, read from the raw bytes at
  ///     `offset`.
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
  /// - Parameters:
  ///   - offset: The offset from the beginning of this span, in bytes.
  ///     `offset` must be nonnegative.
  ///   - type: The type of the instance to create.
  ///   - byteOrder: The order in which the bytes will be decoded.
  /// - Returns: A new value of type `T`, read from `offset`.
  @export(implementation)
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
}

// MARK: - store

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

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
  public func storeBytes<T: BitwiseCopyable>(
    of value: T, toByteOffset offset: Int = 0, as type: T.Type
  ) {
    unsafe _storeBytes(of: value, toByteOffset: offset, as: T.self)
  }

  @unsafe
  @export(implementation) @_transparent
  internal func _storeBytes<T: BitwiseCopyable>(
    of value: T, toByteOffset offset: Int, as type: T.Type
  ) {
    _precondition(
      UInt(bitPattern: offset) <= UInt(bitPattern: _count) &&
      MemoryLayout<T>.size <= (_count &- offset),
      "Byte offset range out of bounds"
    )
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
  public func storeBytes<T: BitwiseCopyable>(
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
  public func storeBytes<T: ConvertibleToBytes & BitwiseCopyable>(
    of value: T, toByteOffset offset: Int, as type: T.Type
  ) {
    unsafe _storeBytes(of: value, toByteOffset: offset, as: T.self)
  }

  /// Stores the given value's bytes to the specified offset into
  /// the span's memory.
  ///
  /// - Parameters:
  ///   - value: The value to store as raw bytes.
  ///   - offset: The offset in bytes into the span's memory at which to begin
  ///       writing the bytes from the value.
  ///   - type: The type of the instance to store.
  ///   - byteOrder: The order in which the bytes will be encoded to the span.
  @export(implementation)
  public func storeBytes<
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
  public func storeBytes<T: BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    unsafe _storeBytes(repeating: repeatedValue, count: count, as: T.self)
  }

  @unsafe
  @export(implementation) @_transparent
  internal func _storeBytes<T: BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    _precondition(
      count &* MemoryLayout<T>.stride <= _count,
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
  public func storeBytes<T: ConvertibleToBytes & BitwiseCopyable>(
    repeating repeatedValue: T, count: Int, as type: T.Type
  ) {
    unsafe _storeBytes(repeating: repeatedValue, count: count, as: T.self)
  }

  /// Stores the given value's bytes repeatedly into this span's memory.
  ///
  /// - Parameters:
  ///   - repeatedValue: The value to store as raw bytes.
  ///   - count: The number of copies of `repeatedValue` to store
  ///      into this span.
  ///   - type: The type of the instance to store repeatedly.
  ///   - byteOrder: The order in which the bytes will be encoded to the span.
  @export(implementation)
  public func storeBytes<
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

// MARK: - sub-spans

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this span.
  ///
  /// - Returns: An `AliasedMutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(_ bounds: Range<Int>) -> Self {
    _precondition(
      UInt(bitPattern: bounds.lowerBound) <= UInt(bitPattern: _count) &&
      UInt(bitPattern: bounds.upperBound) <= UInt(bitPattern: _count),
      "Byte offset range out of bounds"
    )
    return unsafe extracting(unchecked: bounds)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this span.
  ///
  /// - Returns: An `AliasedMutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(unchecked bounds: Range<Int>) -> Self {
    let newStart = unsafe _pointer?.advanced(by: bounds.lowerBound)
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: bounds.count)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this span.
  ///
  /// - Returns: An `AliasedMutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(_ bounds: some RangeExpression<Int>) -> Self {
    extracting(bounds.relative(to: byteOffsets))
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this span.
  ///
  /// - Returns: An `AliasedMutableRawSpan` over the bytes within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(unchecked bounds: ClosedRange<Int>) -> Self {
    let range = unsafe Range(
      _uncheckedBounds: (bounds.lowerBound, bounds.upperBound + 1)
    )
    return unsafe extracting(unchecked: range)
  }

  /// Constructs a new span over all the bytes of this span.
  ///
  /// - Returns: An `AliasedMutableRawSpan` over all the bytes of this span.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(_: UnboundedRange) -> Self {
    self
  }
}

// MARK: - prefixes and suffixes

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// Returns a span containing the initial bytes of this span,
  /// up to the specified maximum length.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(first maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, byteCount)
    let newSpan = unsafe Self(_unchecked: _pointer, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Returns a span over all but the given number of trailing bytes.
  ///
  /// - Parameter k: The number of bytes to drop off the end of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span leaving off the specified number of bytes at the end.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(droppingLast k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of bytes")
    let droppedCount = min(k, byteCount)
    let newSpan = unsafe Self(
      _unchecked: _pointer, byteCount: byteCount &- droppedCount
    )
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Returns a span containing the trailing bytes of the span,
  /// up to the given maximum length.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(last maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, byteCount)
    let newStart = unsafe _pointer?.advanced(by: byteCount &- newCount)
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Returns a span over all but the given number of initial bytes.
  ///
  /// - Parameter k: The number of bytes to drop from the beginning of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span starting after the specified number of bytes.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(droppingFirst k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of bytes")
    let droppedCount = min(k, byteCount)
    let newStart = unsafe _pointer?.advanced(by: droppedCount)
    let newCount = byteCount &- droppedCount
    let newSpan = unsafe Self(_unchecked: newStart, byteCount: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }
}

// MARK: - identity

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// Returns a Boolean value indicating whether two instances refer to the
  /// same memory region.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public func isIdentical(to other: Self) -> Bool {
    unsafe (self._pointer == other._pointer) && (self._count == other._count)
  }

  /// Returns a Boolean value indicating whether two instances refer to the
  /// same memory region.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public func isTriviallyIdentical(to other: Self) -> Bool {
    unsafe (self._pointer == other._pointer) && (self._count == other._count)
  }

  /// Returns the byte offsets within this span where the memory represented
  /// by `other` is located, or `nil` if `other` is not located within this
  /// span.
  @export(implementation)
  public func byteOffsets(of other: borrowing Self) -> Range<Int>? {
    bytes.byteOffsets(of: other.bytes)
  }
}

// MARK: - usage hints

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(_:)")
  public subscript(bounds: Range<Int>) -> Self {
    Builtin.unreachable()
  }

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(_:)")
  public subscript(bounds: some RangeExpression<Int>) -> Self {
    Builtin.unreachable()
  }

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(_:)")
  public subscript(bounds: UnboundedRange) -> Self {
    Builtin.unreachable()
  }

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(first:)")
  public func prefix(_ maxLength: Int) -> Self {
    Builtin.unreachable()
  }

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(last:)")
  public func suffix(_ maxLength: Int) -> Self {
    Builtin.unreachable()
  }

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(droppingFirst:)")
  public func dropFirst(_ k: Int = 1) -> Self {
    Builtin.unreachable()
  }

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(droppingLast:)")
  public func dropLast(_ k: Int = 1) -> Self {
    Builtin.unreachable()
  }
}

// MARK: - description

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {
  @export(implementation)
  public var _description: String {
    let addr = unsafe String(
      UInt(bitPattern: _pointer), radix: 16, uppercase: false
    )
    return "(0x\(addr), \(_count))"
  }
}

// MARK: - Iterable

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan: Iterable {
  @available(SwiftStdlib 6.5, *)
  public typealias Failure = Never

  @export(implementation)
  public var underestimatedCount: Int {
    self.byteCount
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> AliasedSpan<UInt8>.BorrowingIterator {
    .init(AliasedSpan(viewing: self.bytes))
  }
}

// MARK: - conversions

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// An aliased raw span referencing the same bytes as this mutable span.
  ///
  /// Retrieving a non-mutating aliased raw span from an aliased mutable raw
  /// span is a safe operation, because both already assume that the
  /// underlying storage may be aliased.
  @export(implementation)
  @_transparent
  public var bytes: AliasedRawSpan {
    @_lifetime(copy self)
    get {
      let result = unsafe AliasedRawSpan(
        _unchecked: UnsafeRawPointer(_pointer), byteCount: _count
      )
      return unsafe _overrideLifetime(result, copying: self)
    }
  }

  /// A mutable raw span referencing the same bytes as this aliased mutable
  /// raw span.
  ///
  /// Retrieving a `MutableRawSpan` from an `AliasedMutableRawSpan` is an
  /// unsafe operation, because one must ensure that the underlying storage is
  /// not accessed at all (read or write) through any other reference while
  /// the mutable raw span is in use.
  @unsafe
  @export(implementation)
  @_transparent
  public var mutableRawSpan: MutableRawSpan {
    @_lifetime(copy self)
    get {
      let result = unsafe MutableRawSpan(
        _unchecked: _pointer, byteCount: _count
      )
      return unsafe _overrideLifetime(result, copying: self)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension MutableRawSpan {

  /// Retrieve an aliased mutable raw span from this mutable raw span.
  ///
  /// This operation consumes the `MutableRawSpan`, which ensures that the
  /// original span (which assumes exclusivity) cannot be used while the
  /// returned `AliasedMutableRawSpan`, or any copy of it, is still in use.
  @export(implementation)
  @_lifetime(copy self)
  @_transparent
  public consuming func asAliased() -> AliasedMutableRawSpan {
    let result = unsafe AliasedMutableRawSpan(
      _unchecked: _pointer, byteCount: _count
    )
    return unsafe _overrideLifetime(result, copying: self)
  }
}

// MARK: - typed views

@available(SwiftStdlib 6.5, *)
extension AliasedMutableRawSpan {

  /// Unsafely view the bytes of this span as instances of `T`.
  @unsafe
  @export(implementation)
  @_lifetime(copy self)
  public func _unsafeView<T: BitwiseCopyable>(
    as type: T.Type
  ) -> AliasedSpan<T> {
    let bytes = unsafe UnsafeRawBufferPointer(start: _pointer, count: _count)
    let span = unsafe AliasedSpan<T>(_unsafeBytes: bytes)
    return unsafe _overrideLifetime(span, copying: self)
  }

  /// Unsafely view the bytes of this span as mutable instances of `T`.
  @unsafe
  @export(implementation)
  @_lifetime(copy self)
  public func _unsafeMutableView<T: BitwiseCopyable>(
    as type: T.Type
  ) -> AliasedMutableSpan<T> {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _pointer, count: _count
    )
    let span = unsafe AliasedMutableSpan<T>(_unsafeBytes: bytes)
    return unsafe _overrideLifetime(span, copying: self)
  }
}
