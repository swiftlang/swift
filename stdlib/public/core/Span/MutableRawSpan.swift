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
    count: Int
  ) {
    _pointer = pointer
    _count = count
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
    let span = MutableRawSpan(_unchecked: baseAddress, count: bytes.count)
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
    precondition(byteCount >= 0, "Count must not be negative")
    self.init(_unchecked: pointer, count: byteCount)
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
    .init(uncheckedBounds: (0, byteCount))
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
    precondition(
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
    precondition(
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
    precondition(
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
    precondition(
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
