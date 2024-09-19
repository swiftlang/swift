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
public struct Span<Element: ~Copyable & ~Escapable>: Copyable, ~Escapable {
  @usableFromInline let _pointer: UnsafeRawPointer?

  @usableFromInline @inline(__always)
  var _start: UnsafeRawPointer { _pointer.unsafelyUnwrapped }

  @usableFromInline let _count: Int

  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline @inline(__always)
  init(
    _unchecked start: UnsafeRawPointer?,
    count: Int
  ) -> dependsOn(immortal) Self {
    _pointer = .init(start)
    _count = count
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
@available(*, unavailable)
extension Span: Sendable {}

@_disallowFeatureSuppression(NonescapableTypes)
extension Span where Element: ~Copyable {
  @_disallowFeatureSuppression(NonescapableTypes)
  @usableFromInline @inline(__always)
  internal init(
    _unchecked elements: UnsafeBufferPointer<Element>
  ) -> dependsOn(immortal) Self {
    _pointer = .init(elements.baseAddress)
    _count = elements.count
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer` to initialized elements.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeElements buffer: UnsafeBufferPointer<Element>
  ) -> dependsOn(immortal) Self {
    _precondition(
      buffer.count == 0 ||
      ((Int(bitPattern: buffer.baseAddress.unsafelyUnwrapped) &
        (MemoryLayout<Element>.alignment&-1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    self.init(_unchecked: buffer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeElements buffer: UnsafeMutableBufferPointer<Element>
  ) -> dependsOn(immortal) Self {
    self.init(_unsafeElements: UnsafeBufferPointer(buffer))
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory representing `count` instances starting at
  /// `pointer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeStart start: UnsafePointer<Element>,
    count: Int
  ) -> dependsOn(immortal) Self {
    _precondition(count >= 0, "Count must not be negative")
    self.init(_unsafeElements: .init(_uncheckedStart: start, count: count))
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
extension Span where Element: BitwiseCopyable {

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeBufferPointer` to initialized elements.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeElements buffer: UnsafeBufferPointer<Element>
  ) -> dependsOn(immortal) Self {
    self.init(_unchecked: buffer)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `buffer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to initialized elements.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeElements buffer: UnsafeMutableBufferPointer<Element>
  ) -> dependsOn(immortal) Self {
    self.init(_unsafeElements: UnsafeBufferPointer(buffer))
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory representing `count` instances starting at
  /// `pointer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeStart start: UnsafePointer<Element>,
    count: Int
  ) -> dependsOn(immortal) Self {
    _precondition(count >= 0, "Count must not be negative")
    self.init(_unchecked: start, count: count)
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `unsafeBytes` must be owned by the instance `owner`
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// `unsafeBytes` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - unsafeBytes: a buffer to initialized elements.
  ///   - type: the type to use when interpreting the bytes in memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeBytes buffer: UnsafeRawBufferPointer
  ) -> dependsOn(immortal) Self {
    let (byteCount, stride) = (buffer.count, MemoryLayout<Element>.stride)
    _precondition(byteCount >= 0, "Count must not be negative")
    let (count, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
    _precondition(remainder == 0)
    self.init(
      _unchecked: buffer.baseAddress?.assumingMemoryBound(to: Element.self),
      count: count
    )
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory in `unsafeBytes` must be owned by the instance `owner`
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// `unsafeBytes` must be correctly aligned for accessing
  /// an element of type `Element`, and must contain a number of bytes
  /// that is an exact multiple of `Element`'s stride.
  ///
  /// - Parameters:
  ///   - unsafeBytes: a buffer to initialized elements.
  ///   - type: the type to use when interpreting the bytes in memory.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeBytes buffer: UnsafeMutableRawBufferPointer
  ) -> dependsOn(immortal) Self {
    self.init(_unsafeBytes: UnsafeRawBufferPointer(buffer))
  }

  /// Unsafely create a `Span` over initialized memory.
  ///
  /// The memory representing `count` instances starting at
  /// `pointer` must be owned by the instance `owner`,
  /// meaning that as long as `owner` is alive the memory will remain valid.
  ///
  /// - Parameters:
  ///   - pointer: a pointer to the first initialized element.
  ///   - count: the number of initialized elements in the span.
  ///   - owner: a binding whose lifetime must exceed that of
  ///            the newly created `Span`.
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public init(
    _unsafeStart pointer: UnsafeRawPointer,
    byteCount: Int
  ) -> dependsOn(immortal) Self {
    _precondition(byteCount >= 0, "Count must not be negative")
    let stride = MemoryLayout<Element>.stride
    let (count, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
    _precondition(remainder == 0)
    self.init(
      _unchecked: pointer.assumingMemoryBound(to: Element.self),
      count: count
    )
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
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
extension Span where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var description: String {
    let addr = String(UInt(bitPattern: _pointer), radix: 16, uppercase: false)
    return "(0x\(addr), \(_count))"
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
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

  /// The indices that are valid for subscripting the span, in ascending
  /// order.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public var _indices: Range<Int> {
    Range(_uncheckedBounds: (0, _count))
  }
}

//MARK: Bounds Checking
@_disallowFeatureSuppression(NonescapableTypes)
extension Span where Element: ~Copyable {

  /// Return true if `offset` is a valid offset into this `Span`
  ///
  /// - Parameters:
  ///   - position: an index to validate
  /// - Returns: true if `offset` is a valid index
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func boundsContain(_ offset: Int) -> Bool {
    0 <= offset && offset < count
  }

  /// Return true if `offsets` is a valid range of offsets into this `Span`
  ///
  /// - Parameters:
  ///   - offsets: a range of indices to validate
  /// - Returns: true if `offsets` is a valid range of indices
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func boundsContain(_ offsets: Range<Int>) -> Bool {
    0 <= offsets.lowerBound && offsets.upperBound <= count
  }

  /// Return true if `offsets` is a valid range of offsets into this `Span`
  ///
  /// - Parameters:
  ///   - offsets: a range of indices to validate
  /// - Returns: true if `offsets` is a valid range of indices
  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public func boundsContain(_ offsets: ClosedRange<Int>) -> Bool {
    0 <= offsets.lowerBound && offsets.upperBound < count
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
extension Span where Element: BitwiseCopyable {

  /// Construct a RawSpan over the memory represented by this span
  ///
  /// - Returns: a RawSpan over the memory represented by this span
  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe // remove when the lifetime inference is fixed
  @_alwaysEmitIntoClient
  public var _unsafeRawSpan: RawSpan { RawSpan(_unsafeSpan: self) }
}

//MARK: integer offset subscripts
@_disallowFeatureSuppression(NonescapableTypes)
extension Span where Element: ~Copyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(_ position: Int) -> Element {
    _read {
      _precondition(boundsContain(position))
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
  @unsafe
  @_alwaysEmitIntoClient
  public subscript(unchecked position: Int) -> Element {
    _read {
      let element = UnsafeRawPointer(_start).advanced(by: position&*MemoryLayout<Element>.stride)
      let binding = Builtin.bindMemory(element._rawValue, count._builtinWordValue, Element.self)
      defer { Builtin.rebindMemory(element._rawValue, binding) }
      yield UnsafePointer<Element>(element._rawValue).pointee
    }
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
extension Span where Element: BitwiseCopyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(_ position: Int) -> Element {
    get {
      _precondition(boundsContain(position))
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
  @unsafe
  @_alwaysEmitIntoClient
  public subscript(unchecked position: Int) -> Element {
    get {
      let address = UnsafeRawPointer(_start).advanced(by: position&*MemoryLayout<Element>.stride)
      return address.loadUnaligned(as: Element.self)
    }
  }
}

//MARK: extracting sub-spans
@_disallowFeatureSuppression(NonescapableTypes)
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
  @usableFromInline func _extracting(_ bounds: Range<Int>) -> Self {
    _precondition(boundsContain(bounds))
    return _extracting(unchecked: bounds)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(to bounds: Range<Int>) {
    self = _extracting(bounds)
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
  @usableFromInline func _extracting(unchecked bounds: Range<Int>) -> Self {
    Span(
      _unchecked: _pointer?.advanced(by: bounds.lowerBound*MemoryLayout<Element>.stride),
      count: bounds.count
    )
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public mutating func _shrink(toUnchecked bounds: Range<Int>) {
    self = _extracting(unchecked: bounds)
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
  @usableFromInline func _extracting(_ bounds: some RangeExpression<Int>) -> Self {
    _extracting(bounds.relative(to: _indices))
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(to bounds: some RangeExpression<Int>) {
    self = _extracting(bounds)
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
  @usableFromInline func _extracting(
    uncheckedBounds bounds: some RangeExpression<Int>
  ) -> Self {
    _extracting(unchecked: bounds.relative(to: _indices))
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @unsafe
  @_alwaysEmitIntoClient
  public mutating func _shrink(toUnchecked bounds: some RangeExpression<Int>) {
    self = _extracting(uncheckedBounds: bounds)
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
  @usableFromInline func _extracting(_: UnboundedRange) -> Self {
    self
  }
}

//MARK: withUnsafePointer, etc.
@_disallowFeatureSuppression(NonescapableTypes)
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
    guard let pointer = _pointer, count > 0 else {
      return try body(.init(start: nil, count: 0))
    }
    let binding = Builtin.bindMemory(pointer._rawValue, count._builtinWordValue, Element.self)
    defer { Builtin.rebindMemory(pointer._rawValue, binding) }
    return try body(.init(start: .init(pointer._rawValue), count: count))
  }
}

@_disallowFeatureSuppression(NonescapableTypes)
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
extension Span where Element: ~Copyable {
  /// Returns a Boolean value indicating whether two `Span` instances
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
    let stride = MemoryLayout<Element>.stride
    if _isPOD(Element.self) {
      let byteOffset = span._start.distance(to: _start)
      let (lower, r) = byteOffset.quotientAndRemainder(dividingBy: stride)
      guard r == 0 else { return false }
      return lower + _count <= span._count
    } else {
      // we have an alignment precondition, so we can omit a stride check
      let selfEnd = self._start.advanced(by: self._count*stride)
      let spanEnd = span._start.advanced(by: span._count*stride)
      return selfEnd <= spanEnd
    }
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
  public func indicesWithin(_ span: borrowing Self) -> Range<Int>? {
    if _count > span._count { return nil }
    if _count == 0 { return Range(_uncheckedBounds: (0, 0)) }
    if _start < span._start { return nil }
    let stride = MemoryLayout<Element>.stride
    let byteOffset = span._start.distance(to: _start)
    let (lower, r) = byteOffset.quotientAndRemainder(dividingBy: stride)
    guard r == 0 else { return nil }
    let upper = lower + _count
    guard upper <= span._count else { return nil }
    return Range(_uncheckedBounds: (lower, upper))
  }
}

//MARK: one-sided slicing operations
@_disallowFeatureSuppression(NonescapableTypes)
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
  @usableFromInline func _extracting(first maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a prefix of negative length.")
    let newCount = min(maxLength, count)
    return Self(_unchecked: _pointer, count: newCount)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(toFirst maxLength: Int) {
    self = _extracting(first: maxLength)
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
  @usableFromInline func _extracting(droppingLast k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements.")
    let droppedCount = min(k, count)
    return Self(_unchecked: _pointer, count: count&-droppedCount)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(droppingLast k: Int) {
    self = _extracting(droppingLast: k)
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
  @usableFromInline func _extracting(last maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a suffix of negative length.")
    let newCount = min(maxLength, count)
    let newStart = _pointer?.advanced(by: (count&-newCount)*MemoryLayout<Element>.stride)
    return Self(_unchecked: newStart, count: newCount)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(toLast maxLength: Int) {
    self = _extracting(last: maxLength)
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
  @usableFromInline func _extracting(droppingFirst k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements.")
    let droppedCount = min(k, count)
    let newStart = _pointer?.advanced(by: droppedCount*MemoryLayout<Element>.stride)
    return Self(_unchecked: newStart, count: count&-droppedCount)
  }

  @_disallowFeatureSuppression(NonescapableTypes)
  @_alwaysEmitIntoClient
  public mutating func _shrink(droppingFirst k: Int) {
    self = _extracting(droppingFirst: k)
  }
}
