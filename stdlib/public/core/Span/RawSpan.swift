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

// A RawSpan represents a span of initialized memory
// of unspecified type.
@_disallowFeatureSuppression(NonescapableTypes)
@frozen
public struct RawSpan: Copyable, ~Escapable {
  @usableFromInline let _pointer: UnsafeRawPointer?

  @usableFromInline @inline(__always)
  var _start: UnsafeRawPointer { _pointer.unsafelyUnwrapped }

  @usableFromInline let _count: Int

  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline @inline(__always)
  init(
    _unchecked start: UnsafeRawPointer?,
    byteCount: Int
  ) -> dependsOn(immortal) Self {
    _pointer = start
    _count = byteCount
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(*, unavailable)
extension RawSpan: Sendable {}

@_disallowFeatureSuppression(NonescapableTypes)
extension RawSpan {

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `RawSpan`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeBytes buffer: UnsafeRawBufferPointer
  ) -> dependsOn(immortal) Self {
    self.init(
      _unchecked: buffer.baseAddress, byteCount: buffer.count
    )
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableRawBufferPointer` to initialized memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `RawSpan`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeBytes buffer: UnsafeMutableRawBufferPointer
  ) -> dependsOn(immortal) Self {
    self.init(_unsafeBytes: UnsafeRawBufferPointer(buffer))
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory over `count` bytes starting at
  /// `pointer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized byte.
  ///   - byteCount: the number of initialized bytes in the span.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `RawSpan`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeStart pointer: UnsafeRawPointer,
    byteCount: Int
  ) -> dependsOn(immortal) Self {
    _precondition(byteCount >= 0, "Count must not be negative")
    self.init(_unchecked: pointer, byteCount: byteCount)
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeRawBufferPointer` to initialized memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `RawSpan`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init<T: BitwiseCopyable>(
    _unsafeElements buffer: UnsafeBufferPointer<T>
  ) -> dependsOn(immortal) Self {
    self.init(_unsafeBytes: UnsafeRawBufferPointer(buffer))
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory in `buffer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableRawBufferPointer` to initialized memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `RawSpan`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init<T: BitwiseCopyable>(
    _unsafeElements buffer: UnsafeMutableBufferPointer<T>
  ) -> dependsOn(immortal) Self {
    self.init(_unsafeElements: UnsafeBufferPointer(buffer))
  }

  /// Unsafely create a `RawSpan` over initialized memory.
  ///
  /// The memory over `count` bytes starting at
  /// `pointer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized byte.
  ///   - byteCount: the number of initialized bytes in the span.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `RawSpan`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init<T: BitwiseCopyable>(
    _unsafeStart pointer: UnsafePointer<T>,
    count: Int
  ) -> dependsOn(immortal) Self {
    _precondition(count >= 0, "Count must not be negative")
    self.init(
      _unchecked: pointer, byteCount: count*MemoryLayout<T>.stride
    )
  }

  /// Create a `RawSpan` over the memory represented by a `Span<T>`
  ///
  /// - Parameters:
  ///   - span: An existing `Span<T>`, which will define both this
  ///           `RawSpan`'s lifetime and the memory it represents.
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe // remove when fixing the lifetime annotation
  @_alwaysEmitIntoClient
  public init<T: BitwiseCopyable>(
    _unsafeSpan span: borrowing Span<T>
  ) -> dependsOn(immortal) Self {
    self.init(
      _unchecked: UnsafeRawPointer(span._start),
      byteCount: span.count * MemoryLayout<T>.stride
    )
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
extension RawSpan {

  @_alwaysEmitIntoClient
  public var _description: String {
    let addr = String(UInt(bitPattern: _pointer), radix: 16, uppercase: false)
    return "(0x\(addr), \(_count))"
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
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
  public var _byteOffsets: Range<Int> {
    .init(_uncheckedBounds: (0, byteCount))
  }
}

//MARK: Bounds Checking
@_disallowFeatureSuppression(NonescapableTypes)
extension RawSpan {

  /// Return true if `offset` is a valid byte offset into this `RawSpan`
  ///
  /// - Parameters:
  ///   - position: a byte offset to validate
  /// - Returns: true if `offset` is a valid byte offset
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func boundsContain(_ offset: Int) -> Bool {
    0 <= offset && offset < byteCount
  }

  /// Return true if `offsets` is a valid range of offsets into this `RawSpan`
  ///
  /// - Parameters:
  ///   - offsets: a range of byte offsets to validate
  /// - Returns: true if `offsets` is a valid range of byte offsets
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func boundsContain(_ offsets: Range<Int>) -> Bool {
    0 <= offsets.lowerBound && offsets.upperBound <= byteCount
  }

  /// Return true if `offsets` is a valid range of offsets into this `RawSpan`
  ///
  /// - Parameters:
  ///   - offsets: a range of byte offsets to validate
  /// - Returns: true if `offsets` is a valid range of byte offsets
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func boundsContain(_ offsets: ClosedRange<Int>) -> Bool {
    0 <= offsets.lowerBound && offsets.upperBound < byteCount
  }
}

//MARK: extracting sub-spans
@_disallowFeatureSuppression(NonescapableTypes)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline func _extracting(_ bounds: Range<Int>) -> Self {
    _precondition(boundsContain(bounds))
    return _extracting(unchecked: bounds)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(to bounds: Range<Int>) {
    self = _extracting(bounds)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @usableFromInline func _extracting(unchecked bounds: Range<Int>) -> Self {
    RawSpan(
      _unchecked: _pointer?.advanced(by: bounds.lowerBound),
      byteCount: bounds.count
    )
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public mutating func _shrink(toUnchecked bounds: Range<Int>) {
    self = _extracting(unchecked: bounds)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline func _extracting(_ bounds: some RangeExpression<Int>) -> Self {
    _extracting(bounds.relative(to: _byteOffsets))
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(_ bounds: some RangeExpression<Int>) {
    self = _extracting(bounds)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @usableFromInline func _extracting(
    unchecked bounds: some RangeExpression<Int>
  ) -> Self {
    _extracting(unchecked: bounds.relative(to: _byteOffsets))
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public mutating func _shrink(toUnchecked bounds: some RangeExpression<Int>) {
    self = _extracting(unchecked: bounds)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline func _extracting(_: UnboundedRange) -> Self {
    self
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
extension RawSpan {

  //FIXME: mark closure parameter as non-escaping
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func withUnsafeBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    try body(.init(start: (byteCount==0) ? nil : _start, count: byteCount))
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public func _unsafeView<T: BitwiseCopyable>(
    as type: T.Type
  ) -> dependsOn(immortal) Span<T> {
    Span(_unsafeStart: _start, byteCount: byteCount)
  }
}

//MARK: load
@_disallowFeatureSuppression(NonescapableTypes)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public func unsafeLoad<T>(
    fromByteOffset offset: Int = 0, as: T.Type
  ) -> T {
    _precondition(boundsContain(
      Range(_uncheckedBounds: (offset, offset+MemoryLayout<T>.size))
    ))
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public func unsafeLoad<T>(
    fromUncheckedByteOffset offset: Int, as: T.Type
  ) -> T {
    _start.load(fromByteOffset: offset, as: T.self)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public func unsafeLoadUnaligned<T: BitwiseCopyable>(
    fromByteOffset offset: Int = 0, as: T.Type
  ) -> T {
    _precondition(boundsContain(
      Range(_uncheckedBounds: (offset, offset+MemoryLayout<T>.size))
    ))
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public func unsafeLoadUnaligned<T: BitwiseCopyable>(
    fromUncheckedByteOffset offset: Int, as: T.Type
  ) -> T {
    _start.loadUnaligned(fromByteOffset: offset, as: T.self)
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
extension RawSpan {
  /// Returns a Boolean value indicating whether two `RawSpan` instances
  /// refer to the same region in memory.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func isIdentical(to other: Self) -> Bool {
    (self._pointer == other._pointer) && (self._count == other._count)
  }

  /// Returns true if the memory represented by `span` is a subrange of
  /// the memory represented by `self`
  ///
  /// Parameters:
  /// - span: a span of the same type as `self`
  /// Returns: whether `span` is a subrange of `self`
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func isWithin(_ span: borrowing Self) -> Bool {
    if _count > span._count { return false }
    if _count == 0 { return true }
    if _start < span._start { return false }
    let lower = span._start.distance(to: _start)
    return lower + _count <= span._count
  }

  /// Returns the offsets where the memory of `span` is located within
  /// the memory represented by `self`
  ///
  /// Note: `span` must be a subrange of `self`
  ///
  /// Parameters:
  /// - span: a subrange of `self`
  /// Returns: A range of offsets within `self`
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func byteOffsetsWithin(_ span: borrowing Self) -> Range<Int>? {
    if _count > span._count { return nil }
    if _count == 0 { return Range(uncheckedBounds: (0, 0)) }
    if _start < span._start { return nil }
    let lower = span._start.distance(to: _start)
    let upper = lower + _count
    guard upper <= span._count else { return nil }
    return Range(uncheckedBounds: (lower, upper))
  }
}

//MARK: one-sided slicing operations
@_disallowFeatureSuppression(NonescapableTypes)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline func _extracting(first maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a prefix of negative length.")
    let newCount = min(maxLength, byteCount)
    return Self(_unchecked: _pointer, byteCount: newCount)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(toFirst maxLength: Int) {
    self = _extracting(first: maxLength)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline func _extracting(droppingLast k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements.")
    let dc = min(k, byteCount)
    return Self(_unchecked: _pointer, byteCount: byteCount&-dc)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(droppingLast k: Int) {
    self = _extracting(droppingLast: k)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline func _extracting(last maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a suffix of negative length.")
    let newCount = min(maxLength, byteCount)
    let newStart = _pointer?.advanced(by: byteCount&-newCount)
    return Self(_unchecked: newStart, byteCount: newCount)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(toLast maxLength: Int) {
    self = _extracting(last: maxLength)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline func _extracting(droppingFirst k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements.")
    let dc = min(k, byteCount)
    let newStart = _pointer?.advanced(by: dc)
    return Self(_unchecked: newStart, byteCount: byteCount&-dc)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(droppingFirst k: Int) {
    self = _extracting(droppingFirst: k)
  }
}
