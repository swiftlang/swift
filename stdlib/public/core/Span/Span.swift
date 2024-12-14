//===--- Span.swift -------------------------------------------------------===//
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

/// `Span<Element>` represents a contiguous region of memory
/// which contains initialized instances of `Element`.
///
/// A `Span` instance is a non-owning, non-escaping view into memory.
/// When a `Span` is created, it inherits the lifetime of the container
/// owning the contiguous memory, ensuring temporal safety and avoiding
/// use-after-free errors. Operations on `Span` are bounds-checked,
/// ensuring spcial safety and avoiding buffer overflow errors.
@frozen
@available(SwiftStdlib 6.1, *)
public struct Span<Element: ~Copyable & ~Escapable>
: ~Escapable, Copyable, BitwiseCopyable {

  /// The starting address of this `Span`.
  ///
  /// `_pointer` can be `nil` if and only if `_count` equals 0.
  /// Otherwise, `_pointer` must point to memory that will remain
  /// valid and not mutated as long as this `Span` exists.
  /// The memory at `_pointer` must be initialized
  /// as `_count` instances of `Element`.
  @usableFromInline
  internal let _pointer: UnsafeRawPointer?

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeRawPointer {
    _pointer._unsafelyUnwrappedUnchecked
  }

  /// The number of elements in this `Span`.
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

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// `pointer` must point to a region of `count` initialized instances,
  /// or may be `nil` if `count` is 0.
  ///
  /// The region of memory representing `count` instances starting at `pointer`
  /// must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  @_alwaysEmitIntoClient
  @inline(__always)
  @lifetime(borrow pointer)
  internal init(
    _unchecked pointer: UnsafeRawPointer?,
    count: Int
  ) {
    _pointer = pointer
    _count = count
  }
}

@available(SwiftStdlib 6.1, *)
extension Span: @unchecked Sendable where Element: Sendable {}

@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer` to initialized elements.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeElements buffer: UnsafeBufferPointer<Element>
  ) {
    //FIXME: Workaround for https://github.com/swiftlang/swift/issues/77235
    let baseAddress = UnsafeRawPointer(buffer.baseAddress)
    _precondition(
      ((Int(bitPattern: baseAddress) &
        (MemoryLayout<Element>.alignment &- 1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    let span = Span(_unchecked: baseAddress, count: buffer.count)
    // As a trivial value, 'baseAddress' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeElements buffer: UnsafeMutableBufferPointer<Element>
  ) {
    let buf = UnsafeBufferPointer(buffer)
    let span = Span(_unsafeElements: buf)
    // As a trivial value, 'buf' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The region of memory representing `count` instances starting at `pointer`
  /// must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  @_alwaysEmitIntoClient
  @lifetime(borrow pointer)
  public init(
    _unsafeStart pointer: UnsafePointer<Element>,
    count: Int
  ) {
    _precondition(count >= 0, "Count must not be negative")
    let buf = UnsafeBufferPointer(start: pointer, count: count)
    let span = Span(_unsafeElements: buf)
    // As a trivial value, 'buf' does not formally depend on the
    // lifetime of 'pointer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: pointer)
  }
}

@available(SwiftStdlib 6.1, *)
extension Span {

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer` to initialized elements.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeElements buffer: borrowing Slice<UnsafeBufferPointer<Element>>
  ) {
    let buf = UnsafeBufferPointer(rebasing: buffer)
    let span = Span(_unsafeElements: buf)
    // As a trivial value, 'buf' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeElements buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let buf = UnsafeBufferPointer(rebasing: buffer)
    let span = Span(_unsafeElements: buf)
    // As a trivial value, 'buf' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }
}

@available(SwiftStdlib 6.1, *)
extension Span where Element: BitwiseCopyable {

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// `buffer` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer to initialized elements.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: UnsafeRawBufferPointer
  ) {
    //FIXME: Workaround for https://github.com/swiftlang/swift/issues/77235
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
    let span = Span(_unchecked: baseAddress, count: count)
    // As a trivial value, 'baseAddress' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// `buffer` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer to initialized elements.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: UnsafeMutableRawBufferPointer
  ) {
    let rawBuffer = UnsafeRawBufferPointer(buffer)
    let span = Span(_unsafeBytes: rawBuffer)
    // As a trivial value, 'buf' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The region of memory representing the instances starting at `pointer`
  /// must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// `pointer` must be correctly aligned for accessing
  /// an element of type `Element`, and `byteCount`
  /// must be an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - byteCount: the number of initialized elements in the span.
  @_alwaysEmitIntoClient
  @lifetime(borrow pointer)
  public init(
    _unsafeStart pointer: UnsafeRawPointer,
    byteCount: Int
  ) {
    _precondition(byteCount >= 0, "Count must not be negative")
    let rawBuffer = UnsafeRawBufferPointer(start: pointer, count: byteCount)
    let span = Span(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'pointer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: pointer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// `buffer` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer to initialized elements.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeRawBufferPointer>
  ) {
    let rawBuffer = UnsafeRawBufferPointer(rebasing: buffer)
    let span = Span(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid, initialized and immutable
  /// throughout the lifetime of the newly-created `Span`.
  /// Failure to maintain this invariant results in undefined behaviour.
  ///
  /// `buffer` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer to initialized elements.
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let rawBuffer = UnsafeRawBufferPointer(rebasing: buffer)
    let span = Span(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    self = _overrideLifetime(span, borrowing: buffer)
  }

  /// Create a `Span` over the bytes represented by a `RawSpan`
  ///
  /// - Parameters:
  ///   - bytes: An existing `RawSpan`, which will define both this
  ///            `Span`'s lifetime and the memory it represents.
  @_alwaysEmitIntoClient
  @lifetime(bytes)
  public init(_bytes bytes: consuming RawSpan) {
    let rawBuffer =
      UnsafeRawBufferPointer(start: bytes._pointer, count: bytes.byteCount)
    let span = Span(_unsafeBytes: rawBuffer)
    // As a trivial value, 'rawBuffer' does not formally depend on the
    // lifetime of 'bytes'. Make the dependence explicit.
    self = _overrideLifetime(span, copying: bytes)
  }
}

@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {

  /// The number of elements in the span.
  ///
  /// To check whether the span is empty, use its `isEmpty` property
  /// instead of comparing `count` to zero.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public var count: Int { _count }

  /// A Boolean value indicating whether the span is empty.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _count == 0 }

  /// The representation for a position in `Span`.
  public typealias Index = Int

  /// The indices that are valid for subscripting the span, in ascending
  /// order.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public var indices: Range<Index> {
    Range(_uncheckedBounds: (0, _count))
  }
}

@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    //FIXME: change to unsafeRawAddress when ready
    unsafeAddress {
      _precondition(indices.contains(position), "Index out of bounds")
      return _unsafeAddressOfElement(unchecked: position)
    }
  }

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// This subscript does not validate `position`. Using this subscript
  /// with an invalid `position` results in undefined behaviour.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @_alwaysEmitIntoClient
  public subscript(unchecked position: Index) -> Element {
    //FIXME: change to unsafeRawAddress when ready
    unsafeAddress {
      _unsafeAddressOfElement(unchecked: position)
    }
  }

  @unsafe
  @_alwaysEmitIntoClient
  internal func _unsafeAddressOfElement(
    unchecked position: Index
  ) -> UnsafePointer<Element> {
    let elementOffset = position &* MemoryLayout<Element>.stride
    let address = _start().advanced(by: elementOffset)
    return address.assumingMemoryBound(to: Element.self)
  }
}

@available(SwiftStdlib 6.1, *)
extension Span where Element: BitwiseCopyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    get {
      _precondition(
        UInt(bitPattern: position) <  UInt(bitPattern: _count),
        "Index out of bounds"
      )
      return self[unchecked: position]
    }
  }

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// This subscript does not validate `position`. Using this subscript
  /// with an invalid `position` results in undefined behaviour.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @unsafe
  @_alwaysEmitIntoClient
  public subscript(unchecked position: Index) -> Element {
    get {
      let elementOffset = position &* MemoryLayout<Element>.stride
      let address = _start().advanced(by: elementOffset)
      return address.loadUnaligned(as: Element.self)
    }
  }
}

//MARK: sub-spans
@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {

  /// Constructs a new span over the items within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `Span`.
  ///
  /// - Returns: A `Span` over the items within `bounds`
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(_ bounds: Range<Index>) -> Self {
    _precondition(
      UInt(bitPattern: bounds.lowerBound) <= UInt(bitPattern: _count) &&
      UInt(bitPattern: bounds.upperBound) <= UInt(bitPattern: _count),
      "Index range out of bounds"
    )
    return _extracting(unchecked: bounds)
  }

  /// Constructs a new span over the items within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `Span`.
  ///
  /// - Returns: A `Span` over the items within `bounds`
  ///
  /// - Complexity: O(1)
  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(unchecked bounds: Range<Index>) -> Self {
    let delta = bounds.lowerBound &* MemoryLayout<Element>.stride
    let newStart = _pointer?.advanced(by: delta)
    let newSpan = Span(_unchecked: newStart, count: bounds.count)
    // As a trivial value, 'newStart' does not formally depend on the
    // lifetime of 'self'. Make the dependence explicit.
    return _overrideLifetime(newSpan, copying: self)
  }

  /// Constructs a new span over the items within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `Span`.
  ///
  /// - Returns: A `Span` over the items within `bounds`
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(_ bounds: some RangeExpression<Int>) -> Self {
    _extracting(bounds.relative(to: indices))
  }

  /// Constructs a new span over the items within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// This function does not validate `bounds`; this is an unsafe operation.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `Span`.
  ///
  /// - Returns: A `Span` over the items within `bounds`
  ///
  /// - Complexity: O(1)
  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(
    unchecked bounds: some RangeExpression<Int>
  ) -> Self {
    _extracting(unchecked: bounds.relative(to: indices))
  }

  /// Constructs a new span over all the items of this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Returns: A `Span` over all the items of this span.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(_: UnboundedRange) -> Self {
    self
  }
}

//MARK: UnsafeBufferPointer access hatch
@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable  {

  /// Calls a closure with a pointer to the viewed contiguous storage.
  ///
  /// The buffer pointer passed as an argument to `body` is valid only
  /// during the execution of `withUnsafeBufferPointer(_:)`.
  /// Do not store or return the pointer for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeBufferPointer` parameter
  ///   that points to the viewed contiguous storage. If `body` has
  ///   a return value, that value is also used as the return value
  ///   for the `withUnsafeBufferPointer(_:)` method. The closure's
  ///   parameter is valid only for the duration of its execution.
  /// - Returns: The return value of the `body` closure parameter.
  @_alwaysEmitIntoClient
  public func withUnsafeBufferPointer<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    guard let pointer = _pointer else {
      return try body(.init(start: nil, count: 0))
    }
    let binding = Builtin.bindMemory(
      pointer._rawValue, count._builtinWordValue, Element.self
    )
    defer { Builtin.rebindMemory(pointer._rawValue, binding) }
    return try body(.init(start: .init(pointer._rawValue), count: count))
  }
}

@available(SwiftStdlib 6.1, *)
extension Span where Element: BitwiseCopyable {

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
    try body(
      .init(start: _pointer, count: _count * MemoryLayout<Element>.stride)
    )
  }
}

@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {
  /// Returns a Boolean value indicating whether two `Span` instances
  /// refer to the same region in memory.
  @_alwaysEmitIntoClient
  public func isIdentical(to other: Self) -> Bool {
    (self._pointer == other._pointer) && (self._count == other._count)
  }

  /// Returns the indices within `self` where the memory represented by `span`
  /// is located, or `nil` if `span` is not located within `self`.
  ///
  /// Parameters:
  /// - span: a span that may be a subrange of `self`
  /// Returns: A range of indices within `self`, or `nil`
  @_alwaysEmitIntoClient
  public func indices(of other: borrowing Self) -> Range<Index>? {
    if other._count > _count { return nil }
    guard let spanStart = other._pointer, _count > 0 else {
      return _pointer == other._pointer ? Range(_uncheckedBounds: (0, 0)) : nil
    }
    let start = _start()
    let stride = MemoryLayout<Element>.stride
    let spanEnd = spanStart + stride &* other._count
    if spanStart < start || spanEnd > (start + stride &* _count) { return nil }
    let byteOffset = start.distance(to: spanStart)
    let (lower, r) = byteOffset.quotientAndRemainder(dividingBy: stride)
    guard r == 0 else { return nil }
    return Range(_uncheckedBounds: (lower, lower &+ other._count))
  }
}

//MARK: prefixes and suffixes
@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {

  /// Returns a span containing the initial elements of this span,
  /// up to the specified maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the elements.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter maxLength: The maximum number of elements to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` elements.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(first maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, count)
    return Self(_unchecked: _pointer, count: newCount)
  }

  /// Returns a span over all but the given number of trailing elements.
  ///
  /// If the number of elements to drop exceeds the number of elements in
  /// the span, the result is an empty span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter k: The number of elements to drop off the end of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span leaving off the specified number of elements at the end.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(droppingLast k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, count)
    return Self(_unchecked: _pointer, count: count &- droppedCount)
  }

  /// Returns a span containing the final elements of the span,
  /// up to the given maximum length.
  ///
  /// If the maximum length exceeds the length of this span,
  /// the result contains all the elements.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter maxLength: The maximum number of elements to return.
  ///   `maxLength` must be greater than or equal to zero.
  /// - Returns: A span with at most `maxLength` elements.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(last maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, count)
    let offset = (count &- newCount) * MemoryLayout<Element>.stride
    let newStart = _pointer?.advanced(by: offset)
    let newSpan = Span(_unchecked: newStart, count: newCount)
    // As a trivial value, 'newStart' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    return _overrideLifetime(newSpan, copying: self)
  }

  /// Returns a span over all but the given number of initial elements.
  ///
  /// If the number of elements to drop exceeds the number of elements in
  /// the span, the result is an empty span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter k: The number of elements to drop from the beginning of
  ///   the span. `k` must be greater than or equal to zero.
  /// - Returns: A span starting after the specified number of elements.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(self)
  public func _extracting(droppingFirst k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, count)
    let offset = droppedCount * MemoryLayout<Element>.stride
    let newStart = _pointer?.advanced(by: offset)
    let newSpan = Span(_unchecked: newStart, count: count &- droppedCount)
    // As a trivial value, 'newStart' does not formally depend on the
    // lifetime of 'buffer'. Make the dependence explicit.
    return _overrideLifetime(newSpan, copying: self)
  }
}
