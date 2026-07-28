//===--- AliasedSpan.swift ------------------------------------------------===//
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

/// `AliasedSpan<Element>` represents a contiguous region of memory which
/// contains initialized instances of `Element`, and which may be aliased
/// by other references to the same memory.
///
/// Like `Span`, an `AliasedSpan` instance is a non-owning, non-escaping view
/// into memory. When an `AliasedSpan` is created, it inherits the lifetime of
/// the container owning the contiguous memory, ensuring temporal safety and
/// avoiding use-after-free errors. Operations on `AliasedSpan` are
/// bounds-checked, ensuring spatial safety and avoiding buffer overflow errors.
///
/// Unlike `Span`, an `AliasedSpan` does not rely on the Law of Exclusivity:
/// the memory it references may be modified through some other reference
/// while this span is alive. To remain memory-safe in the presence of such
/// aliases, every read produces an independent copy of the element rather
/// than a borrow of the storage. That has two consequences:
///
/// * `Element` must be `Copyable`, and
/// * accesses are more expensive than the corresponding accesses on `Span`.
///
/// Use `AliasedSpan` only when aliasing genuinely cannot be ruled out, such
/// as for shared memory or memory that is also reachable from C or C++ code.
/// Prefer `Span` everywhere else.
@frozen
@safe
@available(SwiftStdlib 6.5, *)
public struct AliasedSpan<Element>: ~Escapable, Copyable, BitwiseCopyable {

  /// The starting address of this `AliasedSpan`.
  ///
  /// If `_count` is zero, `_pointer` may point to valid memory or it may be
  /// `nil`, but no accesses may be performed through it. Otherwise, `_pointer`
  /// must point to initialized memory containing `_count` instances of
  /// `Element`, which must remain valid and initialized for the lifetime of
  /// this `AliasedSpan`. Unlike `Span`, the memory *may* be mutated during
  /// that lifetime.
  @usableFromInline
  internal let _pointer: UnsafeRawPointer?

  @unsafe
  @export(implementation)
  @inline(__always)
  internal func _start() -> UnsafeRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  /// The number of elements in this `AliasedSpan`.
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
  @_disfavoredOverload
  internal init(
    _unchecked pointer: UnsafeRawPointer?,
    count: Int
  ) {
    unsafe _pointer = pointer
    _count = count
  }

  @unsafe
  @export(implementation)
  @_transparent
  @_lifetime(borrow pointer)
  internal init(
    _unchecked pointer: UnsafePointer<Element>,
    count: Int
  ) {
    unsafe _pointer = UnsafeRawPointer(pointer)
    _count = count
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedSpan: @unchecked Sendable
where Element: Sendable & FullyInhabited {}

// MARK: - unsafe construction

@available(SwiftStdlib 6.5, *)
extension AliasedSpan {

  /// Unsafely create an `AliasedSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid and initialized throughout the
  /// lifetime of the newly-created `AliasedSpan`. Unlike `Span`, the memory
  /// is *not* required to be immutable: other references may modify it.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer` to initialized elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeElements buffer: UnsafeBufferPointer<Element>) {
    _precondition(
      buffer._isWellAligned(),
      "baseAddress must be properly aligned to access Element"
    )
    let span = unsafe AliasedSpan(
      _unchecked: UnsafeRawPointer(buffer.baseAddress), count: buffer.count
    )
    // As a trivial value, 'baseAddress' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid and initialized throughout the
  /// lifetime of the newly-created `AliasedSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeElements buffer: UnsafeMutableBufferPointer<Element>) {
    let span = unsafe AliasedSpan(
      _unsafeElements: UnsafeBufferPointer(buffer)
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedSpan` over initialized memory.
  ///
  /// The region of memory representing `count` instances starting at `pointer`
  /// must remain valid and initialized throughout the lifetime of the
  /// newly-created `AliasedSpan`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  @unsafe
  @export(implementation)
  @_lifetime(borrow pointer)
  public init(_unsafeStart pointer: UnsafePointer<Element>, count: Int) {
    _precondition(count >= 0, "Count must not be negative")
    let buffer = unsafe UnsafeBufferPointer(start: pointer, count: count)
    let span = unsafe AliasedSpan(_unsafeElements: buffer)
    self = unsafe _overrideLifetime(span, borrowing: pointer)
  }

  /// Unsafely create an `AliasedSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid and initialized throughout the
  /// lifetime of the newly-created `AliasedSpan`.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer` to initialized elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(
    _unsafeElements buffer: borrowing Slice<UnsafeBufferPointer<Element>>
  ) {
    let rebased = unsafe UnsafeBufferPointer(rebasing: buffer)
    let span = unsafe AliasedSpan(_unsafeElements: rebased)
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid and initialized throughout the
  /// lifetime of the newly-created `AliasedSpan`.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(
    _unsafeElements buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let rebased = unsafe UnsafeBufferPointer(rebasing: buffer)
    let span = unsafe AliasedSpan(_unsafeElements: rebased)
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedSpan where Element: BitwiseCopyable {

  /// Unsafely create an `AliasedSpan` over initialized memory.
  ///
  /// `buffer` must be correctly aligned for accessing an element of type
  /// `Element`, and must contain a number of bytes that is an exact multiple
  /// of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer of initialized elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeBytes buffer: UnsafeRawBufferPointer) {
    let baseAddress = buffer.baseAddress
    _precondition(
      ((Int(bitPattern: baseAddress) &
        (MemoryLayout<Element>.alignment &- 1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    let (byteCount, stride) = (buffer.count, MemoryLayout<Element>.stride)
    let (count, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
    _precondition(
      remainder == 0, "Span must contain a whole number of elements"
    )
    let span = unsafe AliasedSpan(_unchecked: baseAddress, count: count)
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - buffer: a buffer of initialized elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeBytes buffer: UnsafeMutableRawBufferPointer) {
    let span = unsafe AliasedSpan(
      _unsafeBytes: UnsafeRawBufferPointer(buffer)
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedSpan` over initialized memory.
  ///
  /// `pointer` must be correctly aligned for accessing an element of type
  /// `Element`, and `byteCount` must be an exact multiple of `Element`'s
  /// stride.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - byteCount: the number of bytes in the span.
  @unsafe
  @export(implementation)
  @_lifetime(borrow pointer)
  public init(_unsafeStart pointer: UnsafeRawPointer, byteCount: Int) {
    _precondition(byteCount >= 0, "Count must not be negative")
    let buffer = unsafe UnsafeRawBufferPointer(
      start: pointer, count: byteCount
    )
    let span = unsafe AliasedSpan(_unsafeBytes: buffer)
    self = unsafe _overrideLifetime(span, borrowing: pointer)
  }
}

// MARK: - conversion from raw spans

@available(SwiftStdlib 6.5, *)
extension AliasedSpan where Element: ConvertibleFromBytes {

  /// View initialized raw memory as a typed span.
  ///
  /// The `byteCount` of `bytes` must be a multiple of `Element`'s stride,
  /// and the starting address of `bytes` must be well-aligned for the type
  /// of `Element`. If either of these requirements is not met, this
  /// initializer will trap at runtime.
  ///
  /// - Parameters:
  ///   - bytes: An existing `AliasedRawSpan`, which will define both this
  ///            span's lifetime and the memory it represents.
  @export(implementation)
  @_lifetime(copy bytes)
  public init(viewing bytes: AliasedRawSpan) {
    let buffer = unsafe UnsafeRawBufferPointer(
      start: bytes._pointer, count: bytes.byteCount
    )
    let span = unsafe AliasedSpan(_unsafeBytes: buffer)
    self = unsafe _overrideLifetime(span, copying: bytes)
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedSpan where Element == UInt8 {

  /// View initialized raw memory as a span of bytes.
  ///
  /// - Parameters:
  ///   - bytes: An existing `AliasedRawSpan`, which will define both this
  ///            span's lifetime and the memory it represents.
  @export(implementation)
  @_lifetime(copy bytes)
  public init(viewing bytes: AliasedRawSpan) {
    let span = unsafe Self(_unchecked: bytes._pointer, count: bytes._count)
    self = unsafe _overrideLifetime(span, copying: bytes)
  }
}

// MARK: - basic properties

@available(SwiftStdlib 6.5, *)
extension AliasedSpan {

  /// The number of elements in the span.
  ///
  /// To check whether the span is empty, use its `isEmpty` property
  /// instead of comparing `count` to zero.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_semantics("fixed_storage.get_count")
  public var count: Int { _assumeNonNegative(_count) }

  /// A Boolean value indicating whether the span is empty.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_transparent
  public var isEmpty: Bool { _count == 0 }

  /// The representation for an index in `AliasedSpan`.
  public typealias Index = Int

  /// The indices that are valid for subscripting the span, in ascending
  /// order.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public var indices: Range<Index> {
    unsafe Range(_uncheckedBounds: (0, count))
  }
}

// MARK: - element access

@available(SwiftStdlib 6.5, *)
extension AliasedSpan {
  // SILOptimizer looks for fixed_storage.check_index semantics for bounds
  // check optimizations.
  @_semantics("fixed_storage.check_index")
  @inline(__always)
  @export(implementation)
  internal func _checkIndex(_ position: Index) {
    _precondition(indices.contains(position), "Index out of bounds")
  }

  /// Accesses a copy of the element at the specified index in the span.
  ///
  /// Unlike `Span`, this subscript copies the element out of the underlying
  /// storage. The copy ensures that the returned value remains valid even if
  /// another reference to the same storage replaces the element while the
  /// result of this access is still in use.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public subscript(_ position: Index) -> Element {
    @_transparent
    get {
      _checkIndex(position)
      return unsafe self[unchecked: position]
    }
  }

  /// Accesses a copy of the element at the specified index in the span.
  ///
  /// This subscript does not validate `position`. Using this subscript
  /// with an invalid `position` results in undefined behaviour.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  public subscript(unchecked position: Index) -> Element {
    get {
      unsafe UnsafePointer<Element>(
        _unsafeAddressOfElement(unchecked: position)
      ).pointee
    }
  }

  @unsafe
  @export(implementation)
  @_transparent
  internal func _unsafeAddressOfElement(
    unchecked position: Index
  ) -> Builtin.RawPointer {
#if $BuiltinGepProjection
    unsafe Builtin.gepProjection_Word(
      _start()._rawValue, position._builtinWordValue, Element.self
    )
#else
    let elementOffset = position &* MemoryLayout<Element>.stride
    return unsafe _start().advanced(by: elementOffset)._rawValue
#endif
  }
}

// MARK: - bytes

@available(SwiftStdlib 6.5, *)
extension AliasedSpan where Element: ConvertibleToBytes {

  /// A raw span over the memory represented by this span.
  ///
  /// - Returns: An `AliasedRawSpan` over the memory represented by this span.
  @export(implementation)
  @_transparent
  public var bytes: AliasedRawSpan {
    @_lifetime(copy self)
    get {
      AliasedRawSpan(elements: self)
    }
  }
}

// MARK: - sub-spans

@available(SwiftStdlib 6.5, *)
extension AliasedSpan {

  /// Constructs a new span over the items within the supplied range of
  /// indices within this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of indices. Every index in
  ///     this range must be within the bounds of this span.
  ///
  /// - Returns: An `AliasedSpan` over the items within `bounds`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(_ bounds: Range<Index>) -> Self {
    _precondition(
      UInt(bitPattern: bounds.lowerBound) <= UInt(bitPattern: _count) &&
      UInt(bitPattern: bounds.upperBound) <= UInt(bitPattern: _count),
      "Index range out of bounds"
    )
    return unsafe extracting(unchecked: bounds)
  }

  /// Constructs a new span over the items within the supplied range of
  /// indices within this span.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of indices. Every index in
  ///     this range must be within the bounds of this span.
  ///
  /// - Returns: An `AliasedSpan` over the items within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(unchecked bounds: Range<Index>) -> Self {
    let delta = bounds.lowerBound &* MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: delta)
    let newSpan = unsafe Self(_unchecked: newStart, count: bounds.count)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Constructs a new span over the items within the supplied range of
  /// indices within this span.
  ///
  /// - Parameter bounds: A valid range of indices. Every index in
  ///     this range must be within the bounds of this span.
  ///
  /// - Returns: An `AliasedSpan` over the items within `bounds`.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(_ bounds: some RangeExpression<Index>) -> Self {
    extracting(bounds.relative(to: indices))
  }

  /// Constructs a new span over the items within the supplied range of
  /// indices within this span.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of indices. Every index in
  ///     this range must be within the bounds of this span.
  ///
  /// - Returns: An `AliasedSpan` over the items within `bounds`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(unchecked bounds: ClosedRange<Index>) -> Self {
    let range = unsafe Range(
      _uncheckedBounds: (bounds.lowerBound, bounds.upperBound + 1)
    )
    return unsafe extracting(unchecked: range)
  }

  /// Constructs a new span over all the items of this span.
  ///
  /// - Returns: An `AliasedSpan` over all the items of this span.
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
extension AliasedSpan {

  /// Returns a span containing the initial elements of this span,
  /// up to the specified maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the elements.
  ///
  /// - Parameter maxLength: The maximum number of elements to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` elements.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(first maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, count)
    let newSpan = unsafe Self(_unchecked: _pointer, count: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Returns a span over all but the given number of trailing elements.
  ///
  /// If the number of elements to drop exceeds the number of elements in
  /// the span, the result is an empty span.
  ///
  /// - Parameter k: The number of elements to drop off the end of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span leaving off the specified number of elements at the
  ///   end.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(droppingLast k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, count)
    let newSpan = unsafe Self(
      _unchecked: _pointer, count: count &- droppedCount
    )
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Returns a span containing the trailing elements of the span,
  /// up to the given maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the elements.
  ///
  /// - Parameter maxLength: The maximum number of elements to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` elements.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(last maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, count)
    let offset = (count &- newCount) &* MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: offset)
    let newSpan = unsafe Self(_unchecked: newStart, count: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }

  /// Returns a span over all but the given number of initial elements.
  ///
  /// If the number of elements to drop exceeds the number of elements in
  /// the span, the result is an empty span.
  ///
  /// - Parameter k: The number of elements to drop from the beginning of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span starting after the specified number of elements.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  @_lifetime(copy self)
  public func extracting(droppingFirst k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, count)
    let offset = droppedCount &* MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: offset)
    let newCount = count &- droppedCount
    let newSpan = unsafe Self(_unchecked: newStart, count: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
  }
}

// MARK: - UnsafeBufferPointer access hatch

@available(SwiftStdlib 6.5, *)
extension AliasedSpan {

  /// Calls a closure with a pointer to the viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid only
  /// during the execution of `withUnsafeBufferPointer(_:)`.
  /// Do not store or return the pointer for later use.
  ///
  /// - Note: Because the storage may be aliased, its contents may change
  ///   while `body` is executing. Values read through `buffer` must be
  ///   copied before they are used across any operation that could write
  ///   through another alias.
  ///
  /// - Parameter body: A closure with an `UnsafeBufferPointer` parameter
  ///   that points to the viewed contiguous storage.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_transparent
  @safe
  public func withUnsafeBufferPointer<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    let bytes = unsafe UnsafeRawBufferPointer(
      start: _pointer, count: _count &* MemoryLayout<Element>.stride
    )
    return try unsafe bytes.withMemoryRebound(to: Element.self) {
      buffer throws(E) -> Result in
      try unsafe body(buffer)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedSpan where Element: BitwiseCopyable {

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
    let bytes = unsafe UnsafeRawBufferPointer(
      start: _pointer, count: _count &* MemoryLayout<Element>.stride
    )
    return try unsafe body(bytes)
  }
}

// MARK: - identity

@available(SwiftStdlib 6.5, *)
extension AliasedSpan {

  /// Returns a Boolean value indicating whether two instances refer to the
  /// same memory region.
  ///
  /// - Parameter other: A span to compare with this one.
  /// - Returns: Whether `self` and `other` reference the same region
  ///     in memory.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public func isIdentical(to other: Self) -> Bool {
    unsafe (self._pointer == other._pointer) && (self._count == other._count)
  }

  /// Returns a Boolean value indicating whether two instances refer to the
  /// same memory region.
  ///
  /// - Parameter other: A span to compare with this one.
  /// - Returns: Whether `self` and `other` reference the same region
  ///     in memory.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public func isTriviallyIdentical(to other: Self) -> Bool {
    unsafe (self._pointer == other._pointer) && (self._count == other._count)
  }

  /// Returns the indices within this span where the memory represented
  /// by `other` is located, or `nil` if `other` is not located within this
  /// span.
  ///
  /// - Parameters:
  ///   - other: a span that may be a subrange of `self`
  /// - Returns: A range of indices within `self`, or `nil`.
  @export(implementation)
  public func indices(of other: borrowing Self) -> Range<Index>? {
    if other._count > _count { return nil }
    guard let spanStart = unsafe other._pointer, _count > 0 else {
      return unsafe _pointer == other._pointer ? 0..<0 : nil
    }
    let start = unsafe _start()
    let stride = MemoryLayout<Element>.stride
    let spanEnd = unsafe spanStart + stride &* other._count
    if unsafe spanStart < start || spanEnd > (start + stride &* _count) {
      return nil
    }
    let byteOffset = unsafe start.distance(to: spanStart)
    let (lower, r) = byteOffset.quotientAndRemainder(dividingBy: stride)
    guard r == 0 else { return nil }
    return unsafe Range(_uncheckedBounds: (lower, lower &+ other._count))
  }
}

// MARK: - usage hints
//
// `AliasedSpan` is not a `Collection`. We add the following unavailable
// members to redirect users who reach for the `Collection` slicing API
// towards the corresponding `extracting(...)` function.

@available(SwiftStdlib 6.5, *)
extension AliasedSpan {

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(_:)")
  public subscript(bounds: Range<Index>) -> Self {
    Builtin.unreachable()
  }

  @export(implementation)
  @available(*, unavailable, renamed: "extracting(_:)")
  public subscript(bounds: some RangeExpression<Index>) -> Self {
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
extension AliasedSpan {
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
extension AliasedSpan {

  /// An iterator over the elements of an `AliasedSpan`.
  ///
  /// Because the underlying storage may be modified through another alias
  /// while iteration is in progress, this iterator cannot vend a `Span`
  /// directly over that storage. Instead it copies elements into storage it
  /// owns and vends a `Span` over the copies, in the same manner as the
  /// borrowing iterator of an arbitrary `Sequence`.
  @frozen
  @available(SwiftStdlib 6.5, *)
  public struct BorrowingIterator
  : BorrowingIteratorProtocol, ~Copyable, ~Escapable {
    @usableFromInline
    internal let _span: AliasedSpan<Element>

    @usableFromInline
    internal var _position: Int

    /// Storage for the element that the most recent call to `nextSpan`
    /// vended a span over.
    @usableFromInline
    internal var _buffered: Element?

    @available(SwiftStdlib 6.5, *)
    public typealias Failure = Never

    @available(SwiftStdlib 6.5, *)
    @_lifetime(copy elements)
    @inlinable
    public init(_ elements: AliasedSpan<Element>) {
      _span = elements
      _position = 0
      _buffered = nil
    }

    @available(SwiftStdlib 6.5, *)
    @export(implementation)
    @_lifetime(&self)
    @_lifetime(self: copy self)
    public mutating func nextSpan(maxCount: Int) -> Span<Element> {
      _precondition(maxCount >= 0, "Can't have a prefix of negative length")
      guard maxCount > 0, _position < _span.count else {
        _buffered = nil
        return Span()
      }
      _buffered = unsafe _span[unchecked: _position]
      _position &+= 1
      return _buffered._span()
    }

    @available(SwiftStdlib 6.5, *)
    @export(implementation)
    @_lifetime(self: copy self)
    public mutating func skip(by maxOffset: Int) -> Int {
      _precondition(maxOffset >= 0, "Can't skip by a negative offset")
      let c = Swift.min(maxOffset, _span.count &- _position)
      _position &+= c
      return c
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedSpan: Iterable {
  @available(SwiftStdlib 6.5, *)
  public typealias Failure = Never

  @export(implementation)
  public var underestimatedCount: Int {
    self.count
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> BorrowingIterator {
    .init(self)
  }
}

// MARK: - conversions to and from `Span`

@available(SwiftStdlib 6.5, *)
extension Span {

  /// An aliased span referencing the same storage as this span.
  ///
  /// This conversion is always safe: `AliasedSpan` makes strictly fewer
  /// assumptions about the storage than `Span` does.
  @export(implementation)
  @_transparent
  public var aliased: AliasedSpan<Element> {
    @_lifetime(copy self)
    get {
      let result = unsafe AliasedSpan<Element>(
        _unchecked: _pointer, count: _count
      )
      return unsafe _overrideLifetime(result, copying: self)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedSpan {

  /// A span referencing the same storage as this aliased span.
  ///
  /// Retrieving a `Span` from an `AliasedSpan` is an unsafe operation,
  /// because one must ensure that the underlying storage is not modified by
  /// any code while the span (or any copy derived from it) is in use.
  @unsafe
  @export(implementation)
  @_transparent
  public var span: Span<Element> {
    @_lifetime(copy self)
    get {
      let result = unsafe Span<Element>(_unchecked: _pointer, count: _count)
      return unsafe _overrideLifetime(result, copying: self)
    }
  }
}
