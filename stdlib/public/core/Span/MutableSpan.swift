//===--- MutableSpan.swift ------------------------------------------------===//
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

#if SPAN_COMPATIBILITY_STUB
import Swift
#endif

// A MutableSpan<Element> represents a span of memory which
// contains initialized `Element` instances.
@safe
@frozen
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
public struct MutableSpan<Element: ~Copyable>
: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  @usableFromInline
  internal let _count: Int

  @unsafe
  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  @_alwaysEmitIntoClient
  @inline(__always)
  @lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    _count = 0
  }

  @unsafe
  @_unsafeNonescapableResult
  @_alwaysEmitIntoClient
  @lifetime(borrow start)
  internal init(
    _unchecked start: UnsafeMutableRawPointer?,
    count: Int
  ) {
    unsafe _pointer = start
    _count = count
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan: @unchecked Sendable where Element: Sendable {}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow elements)
  internal init(
    _unchecked elements: UnsafeMutableBufferPointer<Element>
  ) {
    unsafe _pointer = .init(elements.baseAddress)
    _count = elements.count
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeElements buffer: UnsafeMutableBufferPointer<Element>
  ) {
    _precondition(
      ((Int(bitPattern: buffer.baseAddress) &
        (MemoryLayout<Element>.alignment &- 1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    let ms = unsafe MutableSpan<Element>(_unchecked: buffer)
    self = unsafe _overrideLifetime(ms, borrowing: buffer)
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow start)
  public init(
    _unsafeStart start: UnsafeMutablePointer<Element>,
    count: Int
  ) {
    _precondition(count >= 0, "Count must not be negative")
    let buffer = unsafe UnsafeMutableBufferPointer(start: start, count: count)
    let ms = unsafe MutableSpan(_unsafeElements: buffer)
    self = unsafe _overrideLifetime(ms, borrowing: start)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan {

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow elements)
  public init(
    _unsafeElements elements: borrowing Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    let rb = unsafe UnsafeMutableBufferPointer(rebasing: elements)
    let ms = unsafe MutableSpan(_unsafeElements: rb)
    self = unsafe _overrideLifetime(ms, borrowing: elements)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: BitwiseCopyable {

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: UnsafeMutableRawBufferPointer
  ) {
    _precondition(
      ((Int(bitPattern: buffer.baseAddress) &
        (MemoryLayout<Element>.alignment &- 1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    let (byteCount, stride) = (buffer.count, MemoryLayout<Element>.stride)
    let (count, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
    _precondition(remainder == 0, "Span must contain a whole number of elements")
    let elements = unsafe UnsafeMutableBufferPointer<Element>(
      start: buffer.baseAddress?.assumingMemoryBound(to: Element.self),
      count: count
    )
    let ms = unsafe MutableSpan(_unsafeElements: elements)
    self = unsafe _overrideLifetime(ms, borrowing: buffer)
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow pointer)
  public init(
    _unsafeStart pointer: UnsafeMutableRawPointer,
    byteCount: Int
  ) {
    _precondition(byteCount >= 0, "Count must not be negative")
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: pointer, count: byteCount
    )
    let ms = unsafe MutableSpan(_unsafeBytes: bytes)
    self = unsafe _overrideLifetime(ms, borrowing: pointer)
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unsafeBytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>
  ) {
    let bytes = unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
    let ms = unsafe MutableSpan(_unsafeBytes: bytes)
    self = unsafe _overrideLifetime(ms, borrowing: buffer)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension Span where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @lifetime(borrow mutableSpan)
  public init(_mutableSpan mutableSpan: borrowing MutableSpan<Element>) {
    let pointer =
      unsafe mutableSpan._pointer?.assumingMemoryBound(to: Element.self)
    let buffer = unsafe UnsafeBufferPointer(
      start: pointer, count: mutableSpan.count
    )
    let span = unsafe Span(_unsafeElements: buffer)
    self = unsafe _overrideLifetime(span, borrowing: mutableSpan)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var span: Span<Element> {
    @lifetime(borrow self)
    borrowing get {
      Span(_mutableSpan: self)
    }
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension RawSpan {

  @_alwaysEmitIntoClient
  @lifetime(borrow mutableSpan)
  public init<Element: BitwiseCopyable>(
    _mutableSpan mutableSpan: borrowing MutableSpan<Element>
  ) {
    let pointer = unsafe mutableSpan._pointer
    let byteCount = mutableSpan.count &* MemoryLayout<Element>.stride
    let buffer = unsafe UnsafeRawBufferPointer(start: pointer, count: byteCount)
    let rawSpan = unsafe RawSpan(_unsafeBytes: buffer)
    self = unsafe _overrideLifetime(rawSpan, borrowing: mutableSpan)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var _description: String {
    let addr = unsafe String(
      UInt(bitPattern: _pointer), radix: 16, uppercase: false
    )
    return "(0x\(addr), \(_count))"
  }
}

//MARK: Collection, RandomAccessCollection
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var count: Int { _count }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _count == 0 }

  public typealias Index = Int

  @_alwaysEmitIntoClient
  public var indices: Range<Index> {
    unsafe Range(_uncheckedBounds: (0, _count))
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: BitwiseCopyable {

  /// Construct a RawSpan over the memory represented by this span
  ///
  /// - Returns: a RawSpan over the memory represented by this span
  @_alwaysEmitIntoClient
  public var bytes: RawSpan {
    @lifetime(borrow self)
    borrowing get {
      RawSpan(_mutableSpan: self)
    }
  }

  /// Construct a MutableRawSpan over the memory represented by this span
  ///
  /// - Returns: a MutableRawSpan over the memory represented by this span
  @_alwaysEmitIntoClient
  public var mutableBytes: MutableRawSpan {
    @lifetime(&self)
    mutating get {
      MutableRawSpan(_elements: &self)
    }
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

  /// Accesses the element at the specified position in the `Span`.
  ///
  /// - Parameter position: The offset of the element to access. `position`
  ///     must be greater or equal to zero, and less than `count`.
  ///
  /// - Complexity: O(1)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    unsafeAddress {
      _precondition(indices.contains(position), "index out of bounds")
      return unsafe UnsafePointer(_unsafeAddressOfElement(unchecked: position))
    }
    @lifetime(self: copy self)
    unsafeMutableAddress {
      _precondition(indices.contains(position), "index out of bounds")
       return unsafe _unsafeAddressOfElement(unchecked: position)
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
  public subscript(unchecked position: Index) -> Element {
    unsafeAddress {
      unsafe UnsafePointer(_unsafeAddressOfElement(unchecked: position))
    }
    @lifetime(self: copy self)
    unsafeMutableAddress {
      unsafe _unsafeAddressOfElement(unchecked: position)
    }
  }

  @unsafe
  @_alwaysEmitIntoClient
  internal func _unsafeAddressOfElement(
    unchecked position: Index
  ) -> UnsafeMutablePointer<Element> {
    let elementOffset = position &* MemoryLayout<Element>.stride
    let address = unsafe _start().advanced(by: elementOffset)
    return unsafe address.assumingMemoryBound(to: Element.self)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func swapAt(_ i: Index, _ j: Index) {
    _precondition(indices.contains(Index(i)))
    _precondition(indices.contains(Index(j)))
    unsafe swapAt(unchecked: i, unchecked: j)
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func swapAt(unchecked i: Index, unchecked j: Index) {
    let pi = unsafe _unsafeAddressOfElement(unchecked: i)
    let pj = unsafe _unsafeAddressOfElement(unchecked: j)
    let temporary = unsafe pi.move()
    unsafe pi.initialize(to: pj.move())
    unsafe pj.initialize(to: consume temporary)
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

  //FIXME: mark closure parameter as non-escaping
  @_alwaysEmitIntoClient
  public func withUnsafeBufferPointer<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe Span(_mutableSpan: self).withUnsafeBufferPointer(body)
  }

  //FIXME: mark closure parameter as non-escaping
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func withUnsafeMutableBufferPointer<
    E: Error, Result: ~Copyable
  >(
    _ body: (UnsafeMutableBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    guard let pointer = unsafe _pointer, count > 0 else {
      return try unsafe body(.init(start: nil, count: 0))
    }
    // bind memory by hand to sidestep alignment concerns
    let binding = Builtin.bindMemory(
      pointer._rawValue, count._builtinWordValue, Element.self
    )
    defer { Builtin.rebindMemory(pointer._rawValue, binding) }
    return try unsafe body(.init(start: .init(pointer._rawValue), count: count))
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: BitwiseCopyable {

  //FIXME: mark closure parameter as non-escaping
  @_alwaysEmitIntoClient
  public func withUnsafeBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe RawSpan(_mutableSpan: self).withUnsafeBytes(body)
  }

  //FIXME: mark closure parameter as non-escaping
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func withUnsafeMutableBytes<E: Error, Result: ~Copyable>(
    _ body: (_ buffer: UnsafeMutableRawBufferPointer) throws(E) -> Result
  ) throws(E) -> Result {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: (_count == 0) ? nil : _pointer,
      count: _count &* MemoryLayout<Element>.stride
    )
    return try unsafe body(bytes)
  }
}

//MARK: bulk-update functions
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func update(repeating repeatedValue: consuming Element) {
    guard !isEmpty else { return }
    unsafe _start().withMemoryRebound(to: Element.self, capacity: count) {
      unsafe $0.update(repeating: repeatedValue, count: count)
    }
  }
}

// MARK: sub-spans
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

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
  @lifetime(&self)
  mutating public func _mutatingExtracting(_ bounds: Range<Index>) -> Self {
    _precondition(
      UInt(bitPattern: bounds.lowerBound) <= UInt(bitPattern: _count) &&
      UInt(bitPattern: bounds.upperBound) <= UInt(bitPattern: _count),
      "Index range out of bounds"
    )
    return unsafe _mutatingExtracting(unchecked: bounds)
  }

  @available(*, deprecated, renamed: "_mutatingExtracting(_:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(_ bounds: Range<Index>) -> Self {
    _mutatingExtracting(bounds)
  }

  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(_ bounds: Range<Index>) -> Self {
    _precondition(
      UInt(bitPattern: bounds.lowerBound) <= UInt(bitPattern: _count) &&
      UInt(bitPattern: bounds.upperBound) <= UInt(bitPattern: _count),
      "Index range out of bounds"
    )
    return unsafe _consumingExtracting(unchecked: bounds)
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
  @lifetime(&self)
  mutating public func _mutatingExtracting(unchecked bounds: Range<Index>) -> Self {
    let delta = bounds.lowerBound &* MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: delta)
    let newSpan = unsafe Self(_unchecked: newStart, count: bounds.count)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
  }

  @unsafe
  @available(*, deprecated, renamed: "_mutatingExtracting(unchecked:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(unchecked bounds: Range<Index>) -> Self {
    unsafe _mutatingExtracting(unchecked: bounds)
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(unchecked bounds: Range<Index>) -> Self {
    let delta = bounds.lowerBound &* MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: delta)
    let newSpan = unsafe Self(_unchecked: newStart, count: bounds.count)
    return unsafe _overrideLifetime(newSpan, copying: self)
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
  @lifetime(&self)
  mutating public func _mutatingExtracting(
    _ bounds: some RangeExpression<Index>
  ) -> Self {
    _mutatingExtracting(bounds.relative(to: indices))
  }

  @available(*, deprecated, renamed: "_mutatingExtracting(_:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(
    _ bounds: some RangeExpression<Index>
  ) -> Self {
    _mutatingExtracting(bounds)
  }

  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(
    _ bounds: some RangeExpression<Index>
  ) -> Self {
    _consumingExtracting(bounds.relative(to: indices))
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
  @lifetime(&self)
  mutating public func _mutatingExtracting(
    unchecked bounds: ClosedRange<Index>
  ) -> Self {
    let range = unsafe Range(
      _uncheckedBounds: (bounds.lowerBound, bounds.upperBound + 1)
    )
    return unsafe _mutatingExtracting(unchecked: range)
  }

  @unsafe
  @available(*, deprecated, renamed: "_mutatingExtracting(unchecked:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(
    unchecked bounds: ClosedRange<Index>
  ) -> Self {
    unsafe _mutatingExtracting(unchecked: bounds)
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(
    unchecked bounds: ClosedRange<Index>
  ) -> Self {
    let range = unsafe Range(
      _uncheckedBounds: (bounds.lowerBound, bounds.upperBound + 1)
    )
    return unsafe _consumingExtracting(unchecked: range)
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
  @lifetime(&self)
  mutating public func _mutatingExtracting(_: UnboundedRange) -> Self {
    let newSpan = unsafe Self(_unchecked: _pointer, count: _count)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
  }

  @available(*, deprecated, renamed: "_mutatingExtracting(_:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(_: UnboundedRange) -> Self {
    _mutatingExtracting(...)
  }

  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(_: UnboundedRange) -> Self {
    self
  }
}

// MARK: prefixes and suffixes
@available(SwiftCompatibilitySpan 5.0, *)
@_originallyDefinedIn(module: "Swift;CompatibilitySpan", SwiftCompatibilitySpan 6.2)
extension MutableSpan where Element: ~Copyable {

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
  @lifetime(&self)
  mutating public func _mutatingExtracting(first maxLength: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, count)
    let newSpan = unsafe Self(_unchecked: _pointer, count: newCount)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  @available(*, deprecated, renamed: "_mutatingExtracting(first:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(first maxLength: Int) -> Self {
    _mutatingExtracting(first: maxLength)
  }

  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(first maxLength: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(maxLength >= 0, "Can't have a prefix of negative length")
    let newCount = min(maxLength, count)
    let newSpan = unsafe Self(_unchecked: _pointer, count: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
#else
    fatalError("Unsupported compiler")
#endif
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
  @lifetime(&self)
  mutating public func _mutatingExtracting(droppingLast k: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, count)
    let newCount = count &- droppedCount
    let newSpan = unsafe Self(_unchecked: _pointer, count: newCount)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  @available(*, deprecated, renamed: "_mutatingExtracting(droppingLast:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(droppingLast k: Int) -> Self {
    _mutatingExtracting(droppingLast: k)
  }

  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(droppingLast k: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, count)
    let newCount = count &- droppedCount
    let newSpan = unsafe Self(_unchecked: _pointer, count: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
#else
    fatalError("Unsupported compiler")
#endif
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
  @lifetime(&self)
  mutating public func _mutatingExtracting(last maxLength: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, count)
    let offset = (count &- newCount) * MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: offset)
    let newSpan = unsafe Self(_unchecked: newStart, count: newCount)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  @available(*, deprecated, renamed: "_mutatingExtracting(last:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(last maxLength: Int) -> Self {
    _mutatingExtracting(last: maxLength)
  }

  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(last maxLength: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(maxLength >= 0, "Can't have a suffix of negative length")
    let newCount = min(maxLength, count)
    let offset = (count &- newCount) * MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: offset)
    let newSpan = unsafe Self(_unchecked: newStart, count: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
#else
    fatalError("Unsupported compiler")
#endif
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
  @lifetime(&self)
  mutating public func _mutatingExtracting(droppingFirst k: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, count)
    let offset = droppedCount * MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: offset)
    let newCount = count &- droppedCount
    let newSpan = unsafe Self(_unchecked: newStart, count: newCount)
    return unsafe _overrideLifetime(newSpan, mutating: &self)
#else
    fatalError("Unsupported compiler")
#endif
  }

  @available(*, deprecated, renamed: "_mutatingExtracting(droppingFirst:)")
  @_alwaysEmitIntoClient
  @lifetime(&self)
  mutating public func extracting(droppingFirst k: Int) -> Self {
    _mutatingExtracting(droppingFirst: k)
  }

  @_alwaysEmitIntoClient
  @lifetime(copy self)
  consuming public func _consumingExtracting(droppingFirst k: Int) -> Self {
#if compiler(>=5.3) && hasFeature(SendableCompletionHandlers)
    _precondition(k >= 0, "Can't drop a negative number of elements")
    let droppedCount = min(k, count)
    let offset = droppedCount * MemoryLayout<Element>.stride
    let newStart = unsafe _pointer?.advanced(by: offset)
    let newCount = count &- droppedCount
    let newSpan = unsafe Self(_unchecked: newStart, count: newCount)
    return unsafe _overrideLifetime(newSpan, copying: self)
#else
    fatalError("Unsupported compiler")
#endif
  }
}
