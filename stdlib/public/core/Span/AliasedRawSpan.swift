//===--- AliasedRawSpan.swift ---------------------------------------------===//
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

/// `AliasedRawSpan` represents a contiguous region of memory which contains
/// initialized bytes, and which may be aliased by other references to the same
/// memory.
///
/// `AliasedRawSpan` is to `RawSpan` what `AliasedSpan` is to `Span`: it
/// provides the same lifetime and bounds safety, but does not depend on the
/// Law of Exclusivity, so the bytes it references may be modified through some
/// other reference while this span is alive.
@frozen
@safe
@available(SwiftStdlib 6.5, *)
public struct AliasedRawSpan: ~Escapable, Copyable, BitwiseCopyable {

  /// The starting address of this `AliasedRawSpan`.
  @usableFromInline
  internal let _pointer: UnsafeRawPointer?

  @unsafe
  @export(implementation)
  @inline(__always)
  internal func _start() -> UnsafeRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  /// The number of bytes in this `AliasedRawSpan`.
  @usableFromInline
  internal let _count: Int

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
    _unchecked pointer: UnsafeRawPointer?,
    byteCount: Int
  ) {
    unsafe _pointer = pointer
    _count = byteCount
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan: @unchecked Sendable {}

// MARK: - unsafe construction

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

  /// Unsafely create an `AliasedRawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid and initialized throughout the
  /// lifetime of the newly-created span. Unlike `RawSpan`, the memory is
  /// *not* required to be immutable.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeBytes buffer: UnsafeRawBufferPointer) {
    let baseAddress = buffer.baseAddress
    let span = unsafe AliasedRawSpan(
      _unchecked: baseAddress, byteCount: buffer.count
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - buffer: a `Slice<UnsafeRawBufferPointer>` to initialized memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeBytes buffer: borrowing Slice<UnsafeRawBufferPointer>) {
    let rebased = unsafe UnsafeRawBufferPointer(rebasing: buffer)
    let span = unsafe AliasedRawSpan(_unsafeBytes: rebased)
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableRawBufferPointer` to initialized memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeBytes buffer: UnsafeMutableRawBufferPointer) {
    let span = unsafe AliasedRawSpan(
      _unsafeBytes: UnsafeRawBufferPointer(buffer)
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - buffer: a `Slice<UnsafeMutableRawBufferPointer>` to initialized
  ///     memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let rebased = UnsafeRawBufferPointer(
      unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
    )
    let span = unsafe AliasedRawSpan(_unsafeBytes: rebased)
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized byte.
  ///   - byteCount: the number of initialized bytes in the span.
  @unsafe
  @export(implementation)
  @_lifetime(borrow pointer)
  public init(_unsafeStart pointer: UnsafeRawPointer, byteCount: Int) {
    _precondition(byteCount >= 0, "Count must not be negative")
    unsafe self.init(_unchecked: pointer, byteCount: byteCount)
  }

  /// Unsafely create an `AliasedRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer<T>` to initialized memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init<T: BitwiseCopyable>(_unsafeElements buffer: UnsafeBufferPointer<T>) {
    let span = unsafe AliasedRawSpan(
      _unsafeBytes: UnsafeRawBufferPointer(buffer)
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer<T>` to initialized memory.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init<T: BitwiseCopyable>(
    _unsafeElements buffer: UnsafeMutableBufferPointer<T>
  ) {
    let span = unsafe AliasedRawSpan(
      _unsafeBytes: UnsafeRawBufferPointer(buffer)
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedRawSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  @unsafe
  @export(implementation)
  @_lifetime(borrow pointer)
  public init<T: BitwiseCopyable>(
    _unsafeStart pointer: UnsafePointer<T>,
    count: Int
  ) {
    _precondition(count >= 0, "Count must not be negative")
    unsafe self.init(
      _unchecked: pointer, byteCount: count &* MemoryLayout<T>.stride
    )
  }
}

// MARK: - conversion from typed aliased spans

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

  /// Unsafely view a typed aliased span as a raw aliased span.
  ///
  /// This is unsafe because `Element` may contain padding bytes, which are
  /// not necessarily initialized.
  ///
  /// - Parameters:
  ///   - span: An existing `AliasedSpan<Element>`, from which this span will
  ///     inherit its lifetime.
  @unsafe
  @export(implementation)
  @_lifetime(copy span)
  public init<Element>(unsafeElements span: AliasedSpan<Element>) {
    let rawSpan = unsafe AliasedRawSpan(
      _unchecked: unsafe span._pointer,
      byteCount: span.count == 1 ? MemoryLayout<Element>.size
                 : span.count &* MemoryLayout<Element>.stride
    )
    self = unsafe _overrideLifetime(rawSpan, copying: span)
  }

  /// View a typed aliased span as a raw aliased span.
  ///
  /// - Parameters:
  ///   - span: An existing `AliasedSpan<Element>`, from which this span will
  ///     inherit its lifetime.
  @export(implementation)
  @_lifetime(copy span)
  public init<Element: ConvertibleToBytes>(elements span: AliasedSpan<Element>) {
    unsafe self = Self.init(unsafeElements: span)
  }
}

// MARK: - basic properties

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

  /// The number of bytes in the span.
  ///
  /// To check whether the span is empty, use its `isEmpty` property
  /// instead of comparing `byteCount` to zero.
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

  /// The byte offsets that are valid for subscripting the span, in ascending
  /// order.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public var byteOffsets: Range<Int> {
    unsafe Range(_uncheckedBounds: (0, byteCount))
  }
}

// MARK: - byte access

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {
  // SILOptimizer looks for fixed_storage.check_index semantics
  // for bounds checking optimizations.
  @_semantics("fixed_storage.check_index")
  @export(implementation) @inline(__always)
  internal func _checkIndex(_ position: Int) {
    _precondition(byteOffsets.contains(position), "Index out of bounds")
  }

  /// Accesses the byte at the specified offset in the span.
  ///
  /// - Parameter byteOffset: The offset of the byte to access. `byteOffset`
  ///     must be greater than or equal to zero, and less than `byteCount`.
  @export(implementation) @inline(__always)
  public subscript(_ byteOffset: Int) -> UInt8 {
    _checkIndex(byteOffset)
    return unsafe self[unchecked: byteOffset]
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
    unsafe unsafeLoad(fromUncheckedByteOffset: byteOffset, as: UInt8.self)
  }
}

// MARK: - sub-spans

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

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
  /// - Returns: An `AliasedRawSpan` over the bytes within `bounds`.
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
  /// - Returns: An `AliasedRawSpan` over the bytes within `bounds`.
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
  /// - Returns: An `AliasedRawSpan` over the bytes within `bounds`.
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
  /// - Returns: An `AliasedRawSpan` over the bytes within `bounds`.
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
  /// - Returns: An `AliasedRawSpan` over all the bytes of this span.
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
extension AliasedRawSpan {

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

// MARK: - UnsafeRawBufferPointer access hatch

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

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
    try unsafe body(.init(start: _pointer, count: byteCount))
  }
}

// MARK: - load

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

  /// Returns a new instance of the given type, constructed from the raw
  /// memory at the specified offset.
  ///
  /// The memory at this pointer plus `offset` must be properly aligned for
  /// accessing `T` and initialized to `T` or another type that is layout
  /// compatible with `T`.
  ///
  /// This is an unsafe operation. Failure to meet the preconditions
  /// above may produce an invalid value of `T`.
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
  /// This is an unsafe operation. Failure to meet the preconditions
  /// above may produce an invalid value of `T`.
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

// MARK: - identity

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

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
  ///
  /// - Parameters:
  ///   - other: a span that may be a subrange of `self`
  /// - Returns: A range of byte offsets within `self`, or `nil`.
  @export(implementation)
  public func byteOffsets(of other: borrowing Self) -> Range<Int>? {
    if other._count > _count { return nil }
    guard let spanStart = unsafe other._pointer, _count > 0 else {
      return unsafe _pointer == other._pointer ? 0..<0 : nil
    }
    let start = unsafe _start()
    let spanEnd = unsafe spanStart + other._count
    if unsafe spanStart < start || (start + _count) < spanEnd { return nil }
    let lower = unsafe start.distance(to: spanStart)
    return unsafe Range(_uncheckedBounds: (lower, lower &+ other._count))
  }
}

// MARK: - usage hints

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

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
extension AliasedRawSpan {
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
extension AliasedRawSpan: Iterable {
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
    .init(AliasedSpan(viewing: self))
  }
}

// MARK: - conversions to and from `RawSpan`

@available(SwiftStdlib 6.5, *)
extension RawSpan {

  /// An aliased raw span referencing the same bytes as this raw span.
  ///
  /// This conversion is always safe: `AliasedRawSpan` makes strictly fewer
  /// assumptions about the storage than `RawSpan` does.
  @export(implementation)
  @_transparent
  public var aliased: AliasedRawSpan {
    @_lifetime(copy self)
    get {
      let result = unsafe AliasedRawSpan(
        _unchecked: _pointer, byteCount: _count
      )
      return unsafe _overrideLifetime(result, copying: self)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedRawSpan {

  /// A raw span referencing the same bytes as this aliased raw span.
  ///
  /// Retrieving a `RawSpan` from an `AliasedRawSpan` is an unsafe operation,
  /// because one must ensure that the underlying storage is not modified by
  /// any code while the span (or any copy derived from it) is in use.
  @unsafe
  @export(implementation)
  @_transparent
  public var rawSpan: RawSpan {
    @_lifetime(copy self)
    get {
      let result = unsafe RawSpan(_unchecked: _pointer, byteCount: _count)
      return unsafe _overrideLifetime(result, copying: self)
    }
  }
}
