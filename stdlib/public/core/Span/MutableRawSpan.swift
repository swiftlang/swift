//===--- MutableRawSpan.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// A MutableRawSpan represents a span of memory which
// contains initialized `Element` instances.
@frozen
@available(SwiftStdlib 6.2, *)
public struct MutableRawSpan: ~Copyable & ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  @usableFromInline
  internal let _count: Int

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    _pointer.unsafelyUnwrapped
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow pointer)
  internal init(
    _unchecked pointer: UnsafeMutableRawPointer?,
    byteCount: Int
  ) {
    _pointer = pointer
    _count = byteCount
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableRawSpan: @unchecked Sendable {}

@available(SwiftStdlib 6.2, *)
extension MutableRawSpan {

  @_alwaysEmitIntoClient
  @lifetime(borrow bytes)
  public init(
    _unsafeBytes bytes: UnsafeMutableRawBufferPointer
  ) {
    let baseAddress = bytes.baseAddress
    let span = MutableRawSpan(_unchecked: baseAddress, byteCount: bytes.count)
    self = _overrideLifetime(span, borrowing: bytes)
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow bytes)
  public init(
    _unsafeBytes bytes: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let rebased = UnsafeMutableRawBufferPointer(rebasing: bytes)
    let span = MutableRawSpan(_unsafeBytes: rebased)
    self = _overrideLifetime(span, borrowing: bytes)
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow pointer)
  public init(
    _unsafeStart pointer: UnsafeMutableRawPointer,
    byteCount: Int
  ) {
    _precondition(byteCount >= 0, "Count must not be negative")
    self.init(_unchecked: pointer, byteCount: byteCount)
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow elements)
  public init<Element: BitwiseCopyable>(
    _unsafeElements elements: UnsafeMutableBufferPointer<Element>
  ) {
    let bytes = UnsafeMutableRawBufferPointer(elements)
    let span = MutableRawSpan(_unsafeBytes: bytes)
    self = _overrideLifetime(span, borrowing: elements)
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow elements)
  public init<Element: BitwiseCopyable>(
    _unsafeElements elements: borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let rebased = UnsafeMutableBufferPointer(rebasing: elements)
    let span = MutableRawSpan(_unsafeElements: rebased)
    self = _overrideLifetime(span, borrowing: elements)
  }

  @_alwaysEmitIntoClient
  @lifetime(elements)
  public init<Element: BitwiseCopyable>(
    _elements elements: consuming MutableSpan<Element>
  ) {
    let bytes = UnsafeMutableRawBufferPointer(
      start: elements._pointer,
      count: elements.count &* MemoryLayout<Element>.stride
    )
    let span = MutableRawSpan(_unsafeBytes: bytes)
    self = _overrideLifetime(span, copying: elements)
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableRawSpan {
  @_alwaysEmitIntoClient
  public var byteCount: Int { _count }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { byteCount == 0 }

  @_alwaysEmitIntoClient
  public var byteOffsets: Range<Int> {
    .init(_uncheckedBounds: (0, byteCount))
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableRawSpan {

  @_alwaysEmitIntoClient
  public func withUnsafeBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    guard let pointer = _pointer, _count > 0 else {
      return try body(.init(start: nil, count: 0))
    }
    return try body(.init(start: pointer, count: _count))
  }

  @_alwaysEmitIntoClient
  public mutating func withUnsafeMutableBytes<E: Error, Result: ~Copyable>(
    _ body: (UnsafeMutableRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    guard let pointer = _pointer, _count > 0 else {
      return try body(.init(start: nil, count: 0))
    }
    return try body(.init(start: pointer, count: _count))
  }
}

@available(SwiftStdlib 6.2, *)
extension RawSpan {

  @_alwaysEmitIntoClient
  @lifetime(borrow mutableSpan)
  public init(_unsafeMutableRawSpan mutableSpan: borrowing MutableRawSpan) {
    let start = mutableSpan._start()
    let span = RawSpan(_unsafeStart: start, byteCount: mutableSpan.byteCount)
    self = _overrideLifetime(span, borrowing: mutableSpan)
  }
}

@available(SwiftStdlib 6.2, *)
extension MutableRawSpan {

  public var bytes: RawSpan {
    @_alwaysEmitIntoClient
    @lifetime(borrow self)
    borrowing get {
      return RawSpan(_unsafeMutableRawSpan: self)
    }
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow self)
  public borrowing func _unsafeView<T: BitwiseCopyable>(
    as type: T.Type
  ) -> Span<T> {
    let bytes = UnsafeRawBufferPointer(start: _pointer, count: _count)
    let span = Span<T>(_unsafeBytes: bytes)
    return _overrideLifetime(span, borrowing: self)
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow self)
  public mutating func _unsafeMutableView<T: BitwiseCopyable>(
    as type: T.Type
  ) -> MutableSpan<T> {
    let bytes = UnsafeMutableRawBufferPointer(start: _pointer, count: _count)
    let span = MutableSpan<T>(_unsafeBytes: bytes)
    return _overrideLifetime(span, mutating: &self)
  }
}

@available(SwiftStdlib 6.2, *)
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

  @_alwaysEmitIntoClient
  public func storeBytes<T: BitwiseCopyable>(
    of value: T, toByteOffset offset: Int = 0, as type: T.Type
  ) {
    _precondition(
      UInt(bitPattern: offset) <= UInt(bitPattern: _count) &&
      MemoryLayout<T>.size <= (_count &- offset),
      "Byte offset range out of bounds"
    )
    storeBytes(of: value, toUncheckedByteOffset: offset, as: type)
  }

  @unsafe
  @_alwaysEmitIntoClient
  public func storeBytes<T: BitwiseCopyable>(
    of value: T, toUncheckedByteOffset offset: Int, as type: T.Type
  ) {
    _start().storeBytes(of: value, toByteOffset: offset, as: type)
  }
}

//MARK: copyMemory
@available(SwiftStdlib 6.2, *)
extension MutableRawSpan {

  @_alwaysEmitIntoClient
  public mutating func update<S: Sequence>(
    startingAt byteOffset: Int = 0,
    from source: S
  ) -> (unwritten: S.Iterator, byteOffset: Int) where S.Element: BitwiseCopyable {
    var iterator = source.makeIterator()
    let offset = update(startingAt: byteOffset, from: &iterator)
    return (iterator, offset)
  }

  @_alwaysEmitIntoClient
  public mutating func update<Element: BitwiseCopyable>(
    startingAt byteOffset: Int = 0,
    from elements: inout some IteratorProtocol<Element>
  ) -> Int {
    var offset = byteOffset
    while offset + MemoryLayout<Element>.stride <= _count {
      guard let element = elements.next() else { break }
      storeBytes(of: element, toUncheckedByteOffset: offset, as: Element.self)
      offset &+= MemoryLayout<Element>.stride
    }
    return offset
  }

  @_alwaysEmitIntoClient
  public mutating func update<C: Collection>(
    startingAt byteOffset: Int = 0,
    fromContentsOf source: C
  ) -> Int where C.Element: BitwiseCopyable {
    let newOffset = source.withContiguousStorageIfAvailable {
      self.update(
        startingAt: byteOffset, fromContentsOf: Span(_unsafeElements: $0)
      )
    }
    if let newOffset { return newOffset }

    var elements = source.makeIterator()
    let lastOffset = update(startingAt: byteOffset, from: &elements)
    _precondition(
      elements.next() == nil,
      "destination span cannot contain every element from source."
    )
    return lastOffset
  }

  @_alwaysEmitIntoClient
  public mutating func update<Element: BitwiseCopyable>(
    startingAt byteOffset: Int = 0,
    fromContentsOf source: Span<Element>
  ) -> Int {
//    update(startingAt: byteOffset, from: source.bytes)
    source.withUnsafeBytes {
      update(startingAt: byteOffset, fromContentsOf: $0)
    }
  }

  @_alwaysEmitIntoClient
  public mutating func update<Element: BitwiseCopyable>(
    startingAt byteOffset: Int = 0,
    fromContentsOf source: borrowing MutableSpan<Element>
  ) -> Int {
//    update(startingAt: byteOffset, from: source.storage.bytes)
    source.withUnsafeBytes {
      update(startingAt: byteOffset, fromContentsOf: $0)
    }
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    startingAt byteOffset: Int = 0,
    from source: RawSpan
  ) -> Int {
    if source.byteCount == 0 { return byteOffset }
    source.withUnsafeBytes {
      _start().advanced(by: byteOffset)
              .copyMemory(from: $0.baseAddress!, byteCount: $0.count)
    }
    return byteOffset &+ source.byteCount
  }

  @_alwaysEmitIntoClient
  public mutating func update(
    startingAt byteOffset: Int = 0,
    from source: borrowing MutableRawSpan
  ) -> Int {
    update(startingAt: byteOffset, from: source.bytes)
  }
}

// MARK: sub-spans
@available(SwiftStdlib 6.2, *)
extension MutableRawSpan {

  /// Constructs a new span over the items within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `MutableSpan`.
  ///
  /// - Returns: A `MutableSpan` over the items within `bounds`
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(borrow self)
  mutating public func _extracting(_ bounds: Range<Int>) -> Self {
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
  ///     this range must be within the bounds of this `MutableSpan`.
  ///
  /// - Returns: A `MutableSpan` over the items within `bounds`
  ///
  /// - Complexity: O(1)
  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow self)
  mutating public func _extracting(unchecked bounds: Range<Int>) -> Self {
    let newStart = _pointer?.advanced(by: bounds.lowerBound)
    let newSpan = Self(_unchecked: newStart, byteCount: bounds.count)
    return _overrideLifetime(newSpan, mutating: &self)
  }

  /// Constructs a new span over the items within the supplied range of
  /// positions within this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Parameter bounds: A valid range of positions. Every position in
  ///     this range must be within the bounds of this `MutableSpan`.
  ///
  /// - Returns: A `MutableSpan` over the items within `bounds`
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(borrow self)
  mutating public func _extracting(
    _ bounds: some RangeExpression<Int>
  ) -> Self {
    _extracting(bounds.relative(to: byteOffsets).clamped(to: byteOffsets))
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
  ///     this range must be within the bounds of this `MutableSpan`.
  ///
  /// - Returns: A `MutableSpan` over the items within `bounds`
  ///
  /// - Complexity: O(1)
  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow self)
  mutating public func _extracting(
    unchecked bounds: some RangeExpression<Int>
  ) -> Self {
    _extracting(
      unchecked: bounds.relative(to: byteOffsets).clamped(to: byteOffsets)
    )
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow self)
  mutating public func _extracting(
    unchecked bounds: ClosedRange<Int>
  ) -> Self {
    let range = Range(
      _uncheckedBounds: (bounds.lowerBound, bounds.upperBound&+1)
    )
    return _extracting(unchecked: range)
  }

  /// Constructs a new span over all the items of this span.
  ///
  /// The returned span's first item is always at offset 0; unlike buffer
  /// slices, extracted spans do not share their indices with the
  /// span from which they are extracted.
  ///
  /// - Returns: A `MutableSpan` over all the items of this span.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  @lifetime(borrow self)
  mutating public func _extracting(_: UnboundedRange) -> Self {
    let newSpan = Self(_unchecked: _start(), byteCount: _count)
    return _overrideLifetime(newSpan, mutating: &self)
  }
}

// MARK: prefixes and suffixes
@available(SwiftStdlib 6.2, *)
extension MutableRawSpan {

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
  @lifetime(borrow self)
  mutating public func _extracting(first maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, byteCount)
    let newSpan = Self(_unchecked: _pointer, byteCount: newCount)
    return _overrideLifetime(newSpan, mutating: &self)
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
  @lifetime(borrow self)
  mutating public func _extracting(droppingLast k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let dropped = min(k, byteCount)
    let newSpan = Self(_unchecked: _pointer, byteCount: byteCount &- dropped)
    return _overrideLifetime(newSpan, mutating: &self)
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
  @lifetime(borrow self)
  mutating public func _extracting(last maxLength: Int) -> Self {
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, byteCount)
    let newStart = _pointer?.advanced(by: byteCount &- newCount)
    let newSpan = Self(_unchecked: newStart, byteCount: newCount)
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
  @lifetime(borrow self)
  mutating public func _extracting(droppingFirst k: Int) -> Self {
    _precondition(k >= 0, "Can't drop a negative number of bytes")
    let dropped = min(k, byteCount)
    let newStart = _pointer?.advanced(by: dropped)
    let newSpan = Self(_unchecked: newStart, byteCount: byteCount &- dropped)
    return _overrideLifetime(newSpan, mutating: &self)
  }
}
