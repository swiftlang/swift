//===--- AliasedMutableSpan.swift -----------------------------------------===//
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

/// `AliasedMutableSpan<Element>` represents a contiguous region of memory
/// which contains initialized instances of `Element` and which can be both
/// read and written, and which may be aliased by other references to the same
/// memory.
///
/// `AliasedMutableSpan` is the mutable counterpart of `AliasedSpan`, in the
/// same way that `MutableSpan` is the mutable counterpart of `Span`. It
/// differs from `MutableSpan` in two important ways:
///
/// * It is `Copyable`. `MutableSpan` is noncopyable specifically to prevent
///   the creation of aliases to its storage; since `AliasedMutableSpan`
///   already accounts for aliases, there is no reason to prevent copies.
///   Consequently, the operations that are `mutating` or `consuming` on
///   `MutableSpan` are non-mutating here.
/// * Element accesses use `get` and `nonmutating set` accessors rather than
///   `borrow` and `mutate` accessors, so `Element` must be `Copyable`.
///
/// Use `AliasedMutableSpan` only when aliasing genuinely cannot be ruled out,
/// such as for shared memory or memory that is also reachable from C or C++
/// code. Prefer `MutableSpan` everywhere else.
@frozen
@safe
@available(SwiftStdlib 6.5, *)
public struct AliasedMutableSpan<Element>
: ~Escapable, Copyable, BitwiseCopyable {

  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  @usableFromInline
  internal let _count: Int

  @unsafe
  @export(implementation)
  @_transparent
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
  @_lifetime(borrow start)
  @_disfavoredOverload
  internal init(
    _unchecked start: UnsafeMutableRawPointer?,
    count: Int
  ) {
    unsafe _pointer = start
    _count = count
  }

  @unsafe
  @export(implementation)
  @_transparent
  @_lifetime(borrow start)
  internal init(
    _unchecked start: UnsafeMutablePointer<Element>,
    count: Int
  ) {
    unsafe _pointer = UnsafeMutableRawPointer(start)
    _count = count
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan: @unchecked Sendable
where Element: Sendable & FullyInhabited {}

// MARK: - unsafe construction

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {

  /// Unsafely create an `AliasedMutableSpan` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid and initialized throughout the
  /// lifetime of the newly-created span. Unlike `MutableSpan`, the memory is
  /// *not* required to be exclusively accessed: other references may read and
  /// write it. Failure to maintain this invariant results in undefined
  /// behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeElements buffer: UnsafeMutableBufferPointer<Element>) {
    _precondition(
      buffer._isWellAligned(),
      "baseAddress must be properly aligned to access Element"
    )
    let span = unsafe AliasedMutableSpan(
      _unchecked: UnsafeMutableRawPointer(buffer.baseAddress),
      count: buffer.count
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedMutableSpan` over initialized memory.
  ///
  /// The region of memory representing `count` instances starting at `start`
  /// must remain valid and initialized throughout the lifetime of the
  /// newly-created span.
  ///
  /// - Parameters:
  ///   - start: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  @unsafe
  @export(implementation)
  @_transparent
  @_lifetime(borrow start)
  public init(
    _unsafeStart start: UnsafeMutablePointer<Element>,
    count: Int
  ) {
    _precondition(count >= 0, "Count must not be negative")
    let buffer = unsafe UnsafeMutableBufferPointer(start: start, count: count)
    let span = unsafe AliasedMutableSpan(_unsafeElements: buffer)
    self = unsafe _overrideLifetime(span, borrowing: start)
  }

  /// Unsafely create an `AliasedMutableSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - elements: an `UnsafeMutableBufferPointer` slice of initialized
  ///     elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow elements)
  public init(
    _unsafeElements elements:
      borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let rebased = unsafe UnsafeMutableBufferPointer(rebasing: elements)
    let span = unsafe AliasedMutableSpan(_unsafeElements: rebased)
    self = unsafe _overrideLifetime(span, borrowing: elements)
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan where Element: BitwiseCopyable {

  /// Unsafely create an `AliasedMutableSpan` over initialized memory.
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
  public init(_unsafeBytes buffer: UnsafeMutableRawBufferPointer) {
    _precondition(
      ((Int(bitPattern: buffer.baseAddress) &
        (MemoryLayout<Element>.alignment &- 1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    let (byteCount, stride) = (buffer.count, MemoryLayout<Element>.stride)
    let (count, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
    _precondition(
      remainder == 0, "Span must contain a whole number of elements"
    )
    let span = unsafe AliasedMutableSpan(
      _unchecked: buffer.baseAddress, count: count
    )
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create an `AliasedMutableSpan` over initialized memory.
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
  public init(_unsafeStart pointer: UnsafeMutableRawPointer, byteCount: Int) {
    _precondition(byteCount >= 0, "Count must not be negative")
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: pointer, count: byteCount
    )
    let span = unsafe AliasedMutableSpan(_unsafeBytes: bytes)
    self = unsafe _overrideLifetime(span, borrowing: pointer)
  }

  /// Unsafely create an `AliasedMutableSpan` over initialized memory.
  ///
  /// - Parameters:
  ///   - buffer: a buffer of initialized elements.
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(_unsafeBytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>) {
    let bytes = unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
    let span = unsafe AliasedMutableSpan(_unsafeBytes: bytes)
    self = unsafe _overrideLifetime(span, borrowing: buffer)
  }
}

// MARK: - conversion from raw spans

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan
where Element: ConvertibleFromBytes & ConvertibleToBytes {

  /// View untyped memory as a typed mutable span.
  ///
  /// The `byteCount` of `mutableBytes` must be a multiple of `Element`'s
  /// stride, and the starting address of `mutableBytes` must be well-aligned
  /// for the type of `Element`. If either of these requirements is not met,
  /// this initializer will trap at runtime.
  ///
  /// - Parameter mutableBytes: A raw span to reinterpret as typed elements.
  @export(implementation)
  @_lifetime(copy mutableBytes)
  public init(mutableBytes: AliasedMutableRawSpan) {
    _precondition(
      unsafe ((Int(bitPattern: mutableBytes._pointer) &
        (MemoryLayout<Element>.alignment &- 1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    let byteCount = mutableBytes.byteCount
    let stride = MemoryLayout<Element>.stride
    let (count, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
    _precondition(
      remainder == 0, "Span must contain a whole number of elements"
    )
    self = unsafe _overrideLifetime(
      AliasedMutableSpan(_unchecked: mutableBytes._pointer, count: count),
      copying: mutableBytes
    )
  }
}

// MARK: - basic properties

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {

  /// The number of elements in the span.
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

  /// The type that represents an index in an `AliasedMutableSpan`.
  public typealias Index = Int

  /// The range of valid indices for subscripting the span.
  ///
  /// - Complexity: O(1)
  @export(implementation)
  public var indices: Range<Index> {
    unsafe Range(_uncheckedBounds: (0, count))
  }
}

// MARK: - element access

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {
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
  /// Unlike `MutableSpan`, this subscript copies the element into and out of
  /// the underlying storage rather than providing direct access to it. The
  /// copy ensures that the accessed value remains valid even if another
  /// reference to the same storage replaces the element concurrently.
  ///
  /// The setter is non-mutating because storing an element does not change
  /// the span itself, only the contents of the storage it references.
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
    @_transparent
    nonmutating set {
      _checkIndex(position)
      unsafe self[unchecked: position] = newValue
    }
  }

  /// Accesses a copy of the element at the specified index in the span.
  ///
  /// This subscript does not validate `position`; this is an unsafe
  /// operation.
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
    nonmutating set {
      let address = unsafe _unsafeAddressOfElement(unchecked: position)
      unsafe UnsafeMutablePointer<Element>(address).pointee = newValue
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

// MARK: - bulk update functions

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {

  /// Exchange the elements at the two given indices.
  ///
  /// - Parameter i: A valid index into this span.
  /// - Parameter j: A valid index into this span.
  @export(implementation)
  public func swapAt(_ i: Index, _ j: Index) {
    _checkIndex(i)
    _checkIndex(j)
    unsafe swapAt(unchecked: i, unchecked: j)
  }

  /// Exchange the elements at the two given indices.
  ///
  /// This function does not validate `i` or `j`; this is an unsafe operation.
  ///
  /// - Parameter i: A valid index into this span.
  /// - Parameter j: A valid index into this span.
  @unsafe
  @export(implementation)
  public func swapAt(unchecked i: Index, unchecked j: Index) {
    let temporary = unsafe self[unchecked: i]
    unsafe self[unchecked: i] = self[unchecked: j]
    unsafe self[unchecked: j] = temporary
  }

  /// Update every element of this span to the given value.
  ///
  /// - Parameter repeatedValue: The value to set for every element.
  @export(implementation)
  public func update(repeating repeatedValue: consuming Element) {
    guard !isEmpty else { return }
    unsafe _start().withMemoryRebound(to: Element.self, capacity: count) {
      unsafe $0.update(repeating: repeatedValue, count: count)
    }
  }
}

// MARK: - UnsafeBufferPointer access hatch

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {

  /// Call a closure with a pointer to the viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid only
  /// during the execution of `withUnsafeBufferPointer(_:)`.
  /// Do not store or return the pointer for later use.
  ///
  /// - Note: Because the storage may be aliased, its contents may change
  ///   while `body` is executing.
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

  /// Call a closure with a pointer to the viewed mutable contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid only
  /// during the execution of `withUnsafeMutableBufferPointer(_:)`.
  /// Do not store or return the pointer for later use.
  ///
  /// - Note: Because the storage may be aliased, its contents may change
  ///   while `body` is executing.
  ///
  /// - Parameter body: A closure with an `UnsafeMutableBufferPointer`
  ///   parameter that points to the viewed contiguous storage.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_transparent
  @safe
  public func withUnsafeMutableBufferPointer<E: Error, Result: ~Copyable>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _pointer, count: _count &* MemoryLayout<Element>.stride
    )
    return try unsafe bytes.withMemoryRebound(to: Element.self) {
      buffer throws(E) -> Result in
      try unsafe body(buffer)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan where Element: BitwiseCopyable {

  /// Calls the given closure with a pointer to the underlying bytes of
  /// the viewed contiguous storage.
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

  /// Calls the given closure with a mutable pointer to the underlying bytes
  /// of the viewed contiguous storage.
  ///
  /// - Parameter body: A closure with an `UnsafeMutableRawBufferPointer`
  ///   parameter that points to the viewed contiguous storage.
  /// - Returns: The return value of the `body` closure parameter.
  @export(implementation)
  @_transparent
  @safe
  public func withUnsafeMutableBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeMutableRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _pointer, count: _count &* MemoryLayout<Element>.stride
    )
    return try unsafe body(bytes)
  }
}

// MARK: - sub-spans

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {

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
  /// - Returns: An `AliasedMutableSpan` over the items within `bounds`.
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
  /// - Returns: An `AliasedMutableSpan` over the items within `bounds`.
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
  /// - Returns: An `AliasedMutableSpan` over the items within `bounds`.
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
  /// - Returns: An `AliasedMutableSpan` over the items within `bounds`.
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
  /// - Returns: An `AliasedMutableSpan` over all the items of this span.
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
extension AliasedMutableSpan {

  /// Returns a span containing the initial elements of this span,
  /// up to the specified maximum length.
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

// MARK: - identity

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {

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

  /// Returns the indices within this span where the memory represented
  /// by `other` is located, or `nil` if `other` is not located within this
  /// span.
  @export(implementation)
  public func indices(of other: borrowing Self) -> Range<Index>? {
    aliased.indices(of: other.aliased)
  }
}

// MARK: - usage hints

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {

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
extension AliasedMutableSpan {
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
extension AliasedMutableSpan: Iterable {
  @available(SwiftStdlib 6.5, *)
  public typealias Failure = Never

  @export(implementation)
  public var underestimatedCount: Int {
    self.count
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> AliasedSpan<Element>.BorrowingIterator {
    .init(self.aliased)
  }
}

// MARK: - conversions

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan {

  /// An aliased span referencing the same storage as this mutable span.
  ///
  /// Retrieving a non-mutating aliased span from an aliased mutable span is a
  /// safe operation, because both already assume that the underlying storage
  /// may be aliased.
  @export(implementation)
  @_transparent
  public var aliased: AliasedSpan<Element> {
    @_lifetime(copy self)
    get {
      let result = unsafe AliasedSpan<Element>(
        _unchecked: UnsafeRawPointer(_pointer), count: _count
      )
      return unsafe _overrideLifetime(result, copying: self)
    }
  }

  /// A mutable span referencing the same storage as this aliased mutable
  /// span.
  ///
  /// Retrieving a `MutableSpan` from an `AliasedMutableSpan` is an unsafe
  /// operation, because one must ensure that the underlying storage is not
  /// accessed at all (read or write) through any other reference while the
  /// mutable span is in use.
  @unsafe
  @export(implementation)
  @_transparent
  public var mutableSpan: MutableSpan<Element> {
    @_lifetime(copy self)
    get {
      let result = unsafe MutableSpan<Element>(
        _unchecked: _pointer, count: _count
      )
      return unsafe _overrideLifetime(result, copying: self)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension MutableSpan {

  /// Retrieve an aliased mutable span from this mutable span.
  ///
  /// This operation consumes the `MutableSpan`, which ensures that the
  /// original span (which assumes exclusivity) cannot be used while the
  /// returned `AliasedMutableSpan`, or any copy of it, is still in use.
  @export(implementation)
  @_lifetime(copy self)
  @_transparent
  public consuming func asAliased() -> AliasedMutableSpan<Element> {
    let result = unsafe AliasedMutableSpan<Element>(
      _unchecked: _pointer, count: _count
    )
    return unsafe _overrideLifetime(result, copying: self)
  }
}

// MARK: - bytes

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan where Element: ConvertibleToBytes {

  /// A raw span over the memory represented by this span.
  ///
  /// - Returns: An `AliasedRawSpan` over the memory represented by this span.
  @export(implementation)
  @_transparent
  public var bytes: AliasedRawSpan {
    @_lifetime(copy self)
    get {
      AliasedRawSpan(elements: self.aliased)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension AliasedMutableSpan
where Element: ConvertibleToBytes & ConvertibleFromBytes {

  /// A mutable raw span over the memory represented by this span.
  ///
  /// - Returns: An `AliasedMutableRawSpan` over the memory represented by
  ///   this span.
  @export(implementation)
  @_transparent
  public var mutableBytes: AliasedMutableRawSpan {
    @_lifetime(copy self)
    get {
      AliasedMutableRawSpan(elements: self)
    }
  }
}
