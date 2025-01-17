//===--- RawSpan.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// `RawSpan` represents a contiguous region of memory
/// which contains initialized bytes.
///
/// A `RawSpan` instance is a non-owning, non-escaping view into memory.
/// When a `RawSpan` is created, it inherits the lifetime of the container
/// owning the contiguous memory, ensuring temporal safety and avoiding
/// use-after-free errors. Operations on `RawSpan` are bounds-checked,
/// ensuring spcial safety and avoiding buffer overflow errors.
@available(SwiftStdlib 6.1, *)
@frozen
public struct RawSpan: ~Escapable, Copyable, BitwiseCopyable {

  /// The starting address of this `RawSpan`.
  ///
  /// `_pointer` can be `nil` if and only if `_count` equals 0.
  /// Otherwise, `_pointer` must point to memory that will remain
  /// valid and not mutated as long as this `Span` exists.
  /// The memory at `_pointer` must consist of `_count` initialized bytes.
  @usableFromInline
  internal let _pointer: UnsafeRawPointer?

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeRawPointer {
    _pointer._unsafelyUnwrappedUnchecked
  }

  /// The number of bytes in this `RawSpan`.
  ///
  /// If `_count` equals 0, then `_pointer` may be either `nil` or valid.
  /// Any `_count` greater than 0 indicates a valid non-nil `_pointer`.
  /// Any `_count` less than 0 is invalid and is undefined behaviour.
  @usableFromInline
  internal let _count: Int

  /// FIXME: Remove once supported old compilers can recognize lifetime dependence 
  @_unsafeNonescapableResult
  @_alwaysEmitIntoClient
  @inline(__always)
  internal init() {
    _pointer = nil
    _count = 0
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// `pointer` must point to a region of `byteCount` initialized bytes,
  /// or may be `nil` if `count` is 0.
  ///
  /// The region of `byteCount` bytes of memory starting at `pointer`
  /// must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized byte.
  ///   - byteCount: the number of initialized bytes in the span.
  @_alwaysEmitIntoClient
  @inline(__always)
  @lifetime(borrow pointer)
  internal init(
    _unchecked pointer: UnsafeRawPointer?,
    byteCount: Int
  ) {
    _pointer = pointer
    _count = byteCount
  }
}

@available(SwiftStdlib 6.1, *)
extension RawSpan: @unchecked Sendable {}

@available(SwiftStdlib 6.1, *)
extension RawSpan {

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: UnsafeRawBufferPointer
  ) {
    let baseAddress = buffer.baseAddress
    let span = RawSpan(_unchecked: baseAddress, byteCount: buffer.count)
    // As a trivial value, 'baseAddress' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeRawBufferPointer>
  ) {
    let rawBuffer = UnsafeRawBufferPointer(rebasing: buffer)
    let span = RawSpan(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: UnsafeMutableRawBufferPointer
  ) {
    let rawBuffer = UnsafeRawBufferPointer(buffer)
    let span = RawSpan(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let rawBuffer =
      UnsafeRawBufferPointer(UnsafeMutableRawBufferPointer(rebasing: buffer))
    let span = RawSpan(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The region of memory representing `byteCount` bytes starting at `pointer`
  /// must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized byte.
  ///   - byteCount: the number of initialized bytes in the span.
  @_alwaysEmitIntoClient
  @lifetime(borrow pointer)
  public init(
    _unsafeStart pointer: UnsafeRawPointer,
    byteCount: Int
  ) {
    _precondition(byteCount >= 0, "Count must not be negative")
    self.init(_unchecked: pointer, byteCount: byteCount)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init<T: BitwiseCopyable>(
    _unsafeElements buffer: UnsafeBufferPointer<T>
  ) {
    let rawBuffer = UnsafeRawBufferPointer(buffer)
    let span = RawSpan(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init<T: BitwiseCopyable>(
    _unsafeElements buffer: borrowing Slice<UnsafeBufferPointer<T>>
  ) {
    let rawBuffer = UnsafeRawBufferPointer(UnsafeBufferPointer(rebasing: buffer))
    let span = RawSpan(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init<T: BitwiseCopyable>(
    _unsafeElements buffer: UnsafeMutableBufferPointer<T>
  ) {
    let rawBuffer = UnsafeRawBufferPointer(buffer)
    let span = RawSpan(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init<T: BitwiseCopyable>(
    _unsafeElements buffer: borrowing Slice<UnsafeMutableBufferPointer<T>>
  ) {
    let rawBuffer =
      UnsafeRawBufferPointer(UnsafeMutableBufferPointer(rebasing: buffer))
    let span = RawSpan(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The region of memory representing `byteCount` bytes starting at `pointer`
  /// must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `RawSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized byte.
  ///   - byteCount: the number of initialized bytes in the span.
  @_alwaysEmitIntoClient
  @lifetime(borrow pointer)
  public init<T: BitwiseCopyable>(
    _unsafeStart pointer: UnsafePointer<T>,
    count: Int
  ) {
    _precondition(count >= 0, "Count must not be negative")
    self.init(
      _unchecked: pointer, byteCount: count * MemoryLayout<T>.stride
    )
  }

  /// Create a `RawSpan` over the memory represented by a `Span<T>`
  ///
  /// - Parameters:
  ///   - span: An existing `Span<T>`, which will define both this
  ///           `RawSpan`'s lifetime and the memory it represents.
  @_alwaysEmitIntoClient
  @lifetime(borrow span)
  public init<Element: BitwiseCopyable>(
    _elements span: borrowing Span<Element>
  ) {
    self.init(
      _unchecked: span._pointer,
      byteCount: span.count &* MemoryLayout<Element>.stride
    )
  }
}

@available(SwiftStdlib 6.1, *)
extension RawSpan {

  /// The number of bytes in the span.
  ///
  /// To check whether the span is empty, use its `isEmpty` property
  /// instead of comparing `count` to zero.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public var byteCount: Int { _count }

  /// A Boolean value indicating whether the span is empty.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public var isEmpty: Bool { byteCount == 0 }

  /// The indices that are valid for subscripting the span, in ascending
  /// order.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public var byteOffsets: Range<Int> {
    .init(_uncheckedBounds: (0, byteCount))
  }
}

//MARK: extracting sub-spans
@available(SwiftStdlib 6.1, *)
extension RawSpan {

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `RawSpan`.
  ///
  /// - Returns: A span over the bytes within `bounds`
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(_ bounds: Range<Int>) -> Self {
    _precondition(
      UInt(bitPattern: bounds.lowerBound) <= UInt(bitPattern: _count) &&
      UInt(bitPattern: bounds.upperBound) <= UInt(bitPattern: _count),
      "Byte offset range out of bounds"
    )
    return _extracting(unchecked: bounds)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `RawSpan`.
  ///
  /// - Returns: A span over the bytes within `bounds`
  ///
  /// - Complexity: O(1)
  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(unchecked bounds: Range<Int>) -> Self {
    let newStart = _pointer?.advanced(by: bounds.lowerBound)
    let newSpan = RawSpan(_unchecked: newStart, byteCount: bounds.count)
    return _overrideLifetime(newSpan, copying: self)
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `RawSpan`.
  ///
  /// - Returns: A span over the bytes within `bounds`
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(_ bounds: some RangeExpression<Int>) -> Self {
    _extracting(bounds.relative(to: byteOffsets))
  }

  /// Constructs a new span over the bytes within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `RawSpan`.
  ///
  /// - Returns: A span over the bytes within `bounds`
  ///
  /// - Complexity: O(1)
  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(
    unchecked bounds: some RangeExpression<Int>
  ) -> Self {
    _extracting(unchecked: bounds.relative(to: byteOffsets))
  }

  /// Constructs a new span over all the bytes of this span.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Returns: A span over all the bytes of this span.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(_: UnboundedRange) -> Self {
    self
  }
}

@available(SwiftStdlib 6.1, *)
extension RawSpan {

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
  @_alwaysEmitIntoClient
  public func withUnsafeBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    try body(.init(start: _pointer, count: byteCount))
  }
}

@available(SwiftStdlib 6.1, *)
extension RawSpan {

  /// View the bytes of this span as type `T`
  ///
  /// This is the equivalent of `unsafeBitCast(_:to:)`. The
  /// underlying bytes must be initialized as type `T`, be
  /// initialized to a type that is layout-compatible with `T`,
  /// or the function mapping bit patterns of length `8*MemoryLayout<T>.size`
  /// to instances of `T` must be surjective.
  ///
  /// This is an unsafe operation. Failure to meet the preconditions
  /// above may produce invalid values of `T`.
  ///
  /// - Parameters:
  ///   - type: The type as which to view the bytes of this span.
  /// - Returns: A typed span viewing these bytes as instances of `T`.
  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(self)
  consuming public func _unsafeView<T: BitwiseCopyable>(
    as type: T.Type
  ) -> Span<T> {
    let rawBuffer = UnsafeRawBufferPointer(start: _pointer, count: _count)
    let newSpan = Span<T>(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'self'. Make the dependence explicit.
    return _overrideLifetime(newSpan, copying: self)
  }
}

//MARK: load
@available(SwiftStdlib 6.1, *)
extension RawSpan {

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
  @_alwaysEmitIntoClient
  public func unsafeLoad<T>(
    fromByteOffset offset: Int = 0, as: T.Type
  ) -> T {
    _precondition(
      UInt(bitPattern: offset) <= UInt(bitPattern: _count) &&
      MemoryLayout<T>.size <= (_count &- offset),
      "Byte offset range out of bounds"
    )
    return unsafeLoad(fromUncheckedByteOffset: offset, as: T.self)
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
  @_alwaysEmitIntoClient
  public func unsafeLoad<T>(
    fromUncheckedByteOffset offset: Int, as: T.Type
  ) -> T {
    _start().load(fromByteOffset: offset, as: T.self)
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
  @_alwaysEmitIntoClient
  public func unsafeLoadUnaligned<T: BitwiseCopyable>(
    fromByteOffset offset: Int = 0, as: T.Type
  ) -> T {
    _precondition(
      UInt(bitPattern: offset) <= UInt(bitPattern: _count) &&
      MemoryLayout<T>.size <= (_count &- offset),
      "Byte offset range out of bounds"
    )
    return unsafeLoadUnaligned(fromUncheckedByteOffset: offset, as: T.self)
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
  @_alwaysEmitIntoClient
  public func unsafeLoadUnaligned<T: BitwiseCopyable>(
    fromUncheckedByteOffset offset: Int, as: T.Type
  ) -> T {
    _start().loadUnaligned(fromByteOffset: offset, as: T.self)
  }
}

@available(SwiftStdlib 6.1, *)
extension RawSpan {
  /// Returns a Boolean value indicating whether two `RawSpan` instances
  /// refer to the same region in memory.
  @_alwaysEmitIntoClient
  public func isIdentical(to other: Self) -> Bool {
    (self._pointer == other._pointer) && (self._count == other._count)
  }

  /// Returns the offsets where the memory of `span` is located within
  /// the memory represented by `self`
  ///
  /// Note: `span` must be a subrange of `self`
  ///
  /// Parameters:
  /// - span: a subrange of `self`
  /// Returns: A range of offsets within `self`
  @_alwaysEmitIntoClient
  public func byteOffsets(of other: borrowing Self) -> Range<Int>? {
    if other._count > _count { return nil }
    guard let spanStart = other._pointer, _count > 0 else {
      return _pointer == other._pointer ? Range(_uncheckedBounds: (0, 0)) : nil
    }
    let start = _start()
    let spanEnd = spanStart + other._count
    if spanStart < start || (start + _count) < spanEnd { return nil }
    let lower = start.distance(to: spanStart)
    return Range(_uncheckedBounds: (lower, lower &+ other._count))
  }
}

//MARK: prefixes and suffixes
@available(SwiftStdlib 6.1, *)
extension RawSpan {

  /// Returns a span containing the initial bytes of this span,
  /// up to the specified maximum byte count.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the bytes.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(first maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, byteCount)
    return Self(_unchecked: _pointer, byteCount: newCount)
  }

  /// Returns a span over all but the given number of trailing bytes.
  ///
  /// If the number of elements to drop exceeds the number of elements in
  /// the span, the result is an empty span.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter k: The number of bytes to drop off the end of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span leaving off the specified number of bytes at the end.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(droppingLast k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, byteCount)
    return Self(_unchecked: _pointer, byteCount: byteCount &- droppedCount)
  }

  /// Returns a span containing the trailing bytes of the span,
  /// up to the given maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the bytes.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter maxLength: The maximum number of bytes to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` bytes.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(last maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, byteCount)
    let newStart = _pointer?.advanced(by: byteCount &- newCount)
    let newSpan = RawSpan(_unchecked: newStart, byteCount: newCount)
    // As a trivial value, 'newStart' does not formally depend on the
    // lifetime of 'self'. Make the dependence explicit.
    return _overrideLifetime(newSpan, copying: self)
  }

  /// Returns a span over all but the given number of initial bytes.
  ///
  /// If the number of elements to drop exceeds the number of bytes in
  /// the span, the result is an empty span.
  ///
  /// The returned span's first byte is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter k: The number of bytes to drop from the beginning of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span starting after the specified number of bytes.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(droppingFirst k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, byteCount)
    let newStart = _pointer?.advanced(by: droppedCount)
    let newSpan = RawSpan(_unchecked: newStart, byteCount: byteCount &- droppedCount)
    // As a trivial value, 'newStart' does not formally depend on the
    // lifetime of 'self'. Make the dependence explicit.
    return _overrideLifetime(newSpan, copying: self)
  }
}
