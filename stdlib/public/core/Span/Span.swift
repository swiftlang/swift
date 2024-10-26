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

// A Span<Element> represents a span of memory which
// contains initialized instances of `Element`.
@_disallowFeatureSuppression(NonescapableTypes)
@frozen
@available(SwiftStdlib 6.1, *)
public struct Span<Element: ~Copyable & ~Escapable>
: ~Escapable, Copyable, BitwiseCopyable {
  @usableFromInline internal let _pointer: UnsafeRawPointer?

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeRawPointer {
    _pointer._unsafelyUnwrappedUnchecked
  }

  @usableFromInline internal let _count: Int

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow pointer) rdar://138672380
  @lifetime(immortal)
  internal init(
    _unchecked pointer: borrowing UnsafeRawPointer?,
    count: Int
  ) {
    _pointer = copy pointer
    _count = count
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span: @unchecked Sendable where Element: Sendable {}

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer` to initialized elements.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeElements buffer: borrowing UnsafeBufferPointer<Element>
  ) {
    let baseAddress = buffer.baseAddress //FIXME: rdar://138665760
    _precondition(
      ((Int(bitPattern: baseAddress) &
        (MemoryLayout<Element>.alignment &- 1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    self.init(_unchecked: baseAddress, count: buffer.count)
  }

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeElements buffer: borrowing UnsafeMutableBufferPointer<Element>
  ) {
    self.init(_unsafeElements: UnsafeBufferPointer(buffer))
  }

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory representing `count` instances starting at
  /// `pointer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeStart pointer: borrowing UnsafePointer<Element>,
    count: Int
  ) {
    _precondition(count >= 0, "Count must not be negative")
    self.init(_unsafeElements: .init(start: copy pointer, count: count))
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span {

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer` to initialized elements.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeElements buffer: borrowing Slice<UnsafeBufferPointer<Element>>
  ) {
    self.init(_unsafeElements: UnsafeBufferPointer(rebasing: buffer))
  }

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeElements buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    self.init(_unsafeElements: UnsafeBufferPointer(rebasing: buffer))
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span where Element: BitwiseCopyable {

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// `buffer` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer to initialized elements.
  ///   - type: the type to use when interpreting the bytes in memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeBytes buffer: borrowing UnsafeRawBufferPointer
  ) {
    let baseAddress = buffer.baseAddress //FIXME: rdar://138665760
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
    self.init(_unchecked: baseAddress, count: count)
  }

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// `buffer` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer to initialized elements.
  ///   - type: the type to use when interpreting the bytes in memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeBytes buffer: borrowing UnsafeMutableRawBufferPointer
  ) {
    self.init(_unsafeBytes: UnsafeRawBufferPointer(buffer))
  }

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory representing `count` instances starting at
  /// `pointer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// `pointer` must be correctly aligned for accessing
  /// an element of type `Element`, and `byteCount`
  /// must be an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - byteCount: the number of initialized elements in the span.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeStart pointer: borrowing UnsafeRawPointer,
    byteCount: Int
  ) {
    _precondition(byteCount >= 0, "Count must not be negative")
    self.init(_unsafeBytes: .init(start: copy pointer, count: byteCount))
  }

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// `buffer` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer to initialized elements.
  ///   - type: the type to use when interpreting the bytes in memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeRawBufferPointer>
  ) {
    self.init(_unsafeBytes: UnsafeRawBufferPointer(rebasing: buffer))
  }

  /// Unsafely creates a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime of
  /// the newly-created `Span`.
  ///
  /// `buffer` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - buffer: a buffer to initialized elements.
  ///   - type: the type to use when interpreting the bytes in memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  //FIXME: should be @lifetime(borrow <argname>) rdar://138672380
  @lifetime(immortal)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    self.init(_unsafeBytes: UnsafeRawBufferPointer(rebasing: buffer))
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span where Element: Equatable {

  /// Returns a Boolean value indicating whether this and another span
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A span to compare to this one.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: Self) -> Bool {
    guard count == other.count else { return false }
    if count == 0 { return true }

    //FIXME: This could be short-cut
    //       with a layout constraint where stride equals size,
    //       as long as there is at most 1 unused bit pattern.
    // if Element is BitwiseEquatable {
    // return _swift_stdlib_memcmp(lhs.baseAddress, rhs.baseAddress, count) == 0
    // }
    for o in 0..<count {
      if self[unchecked: o] != other[unchecked: o] { return false }
    }
    return true
  }

  /// Returns a Boolean value indicating whether this span and a Collection
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A Collection to compare to this span.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: some Collection<Element>) -> Bool {
    let equal = other.withContiguousStorageIfAvailable {
      _elementsEqual(Span(_unsafeElements: $0))
    }
    if let equal { return equal }

    guard count == other.count else { return false }
    if count == 0 { return true }

    return _elementsEqual(AnySequence(other))
  }

  /// Returns a Boolean value indicating whether this span and a Sequence
  /// contain equal elements in the same order.
  ///
  /// - Parameters:
  ///   - other: A Sequence to compare to this span.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _elementsEqual(_ other: some Sequence<Element>) -> Bool {
    var offset = 0
    for otherElement in other {
      if offset >= count { return false }
      if self[unchecked: offset] != otherElement { return false }
      offset += 1
    }
    return offset == count
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public var indices: Range<Index> {
    Range(_uncheckedBounds: (0, _count))
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    //FIXME: change to unsafeRawAddress or unsafeAddress when ready
    _read {
      _precondition(indices.contains(position), "index out of bounds")
      yield self[unchecked: position]
    }
  }

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// This subscript does not validate `position`; this is an unsafe operation.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public subscript(unchecked position: Index) -> Element {
    //FIXME: change to unsafeRawAddress or unsafeAddress when ready
    _read {
      let elementOffset = position &* MemoryLayout<Element>.stride
      let address = _start().advanced(by: elementOffset)
      let binding = Builtin.bindMemory(
        address._rawValue, 1._builtinWordValue, Element.self
      )
      defer { Builtin.rebindMemory(address._rawValue, binding) }
      yield UnsafePointer<Element>(address._rawValue).pointee
    }
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span where Element: BitwiseCopyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    get {
      _precondition(
        UInt(bitPattern: position) <  UInt(bitPattern: _count),
        "index out of bounds"
      )
      return self[unchecked: position]
    }
  }

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// This subscript does not validate `position`; this is an unsafe operation.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_disallowFeatureSuppression(NonescapableTypes)
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
@_disallowFeatureSuppression(NonescapableTypes)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _extracting(_ bounds: Range<Index>) -> Self {
    _precondition(
      UInt(bitPattern: bounds.lowerBound) <= UInt(bitPattern: _count) &&
      UInt(bitPattern: bounds.upperBound) <= UInt(bitPattern: _count),
      "index range out of bounds"
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public func _extracting(unchecked bounds: Range<Index>) -> Self {
    let delta = bounds.lowerBound &* MemoryLayout<Element>.stride
    return Span(_unchecked: _pointer?.advanced(by: delta), count: bounds.count)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public func _extracting(
    uncheckedBounds bounds: some RangeExpression<Int>
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _extracting(_: UnboundedRange) -> Self {
    self
  }
}

//MARK: UnsafeBufferPointer access hatch
@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable  {

  //FIXME: mark closure parameter as non-escaping
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
  @_disallowFeatureSuppression(NonescapableTypes)
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

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span where Element: BitwiseCopyable {

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
    try RawSpan(_unsafeSpan: self).withUnsafeBytes(body)
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(SwiftStdlib 6.1, *)
extension Span where Element: ~Copyable {
  /// Returns a Boolean value indicating whether two `Span` instances
  /// refer to the same region in memory.
  @_disallowFeatureSuppression(NonescapableTypes)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func indices(of span: borrowing Self) -> Range<Index>? {
    if span._count > _count { return nil }
    guard let spanStart = span._pointer, _count > 0 else {
      return _pointer == span._pointer ? Range(_uncheckedBounds: (0, 0)) : nil
    }
    let start = _start()
    let stride = MemoryLayout<Element>.stride
    let spanEnd = spanStart + stride &* span._count
    if spanStart < start || spanEnd > (start + stride &* _count) { return nil }
    let byteOffset = start.distance(to: spanStart)
    let (lower, r) = byteOffset.quotientAndRemainder(dividingBy: stride)
    guard r == 0 else { return nil }
    return Range(_uncheckedBounds: (lower, lower &+ span._count))
  }
}

//MARK: prefixes and suffixes
@_disallowFeatureSuppression(NonescapableTypes)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _extracting(first maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a prefix of negative length.")
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _extracting(droppingLast k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements.")
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _extracting(last maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a suffix of negative length.")
    let newCount = min(maxLength, count)
    let offset = (count &- newCount) * MemoryLayout<Element>.stride
    let newStart = _pointer?.advanced(by: offset)
    return Self(_unchecked: newStart, count: newCount)
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
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func _extracting(droppingFirst k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements.")
    let droppedCount = min(k, count)
    let offset = droppedCount * MemoryLayout<Element>.stride
    let newStart = _pointer?.advanced(by: offset)
    return Self(_unchecked: newStart, count: count &- droppedCount)
  }
}
