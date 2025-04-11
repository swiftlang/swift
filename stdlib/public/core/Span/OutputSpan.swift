//===--- OutputSpan.swift -------------------------------------------------===//
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

// OutputSpan<Element> represents a span of memory which contains
// a variable number of `Element` instances, followed by uninitialized memory.
@safe
@frozen
@available(SwiftStdlib 6.2, *)
public struct OutputSpan<Element: ~Copyable>: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  public let capacity: Int

  @usableFromInline
  internal var _count: Int = 0

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  @_alwaysEmitIntoClient
  public var freeCapacity: Int { capacity &- _count }

  @_alwaysEmitIntoClient
  public var count: Int { _count }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _count == 0 }

  @_alwaysEmitIntoClient
  public var isFull: Bool { _count == capacity }

  deinit {
    if _count > 0 {
      unsafe _start().withMemoryRebound(
        to: Element.self, capacity: _count
      ) {
        [ workaround = _count ] in
        _ = unsafe $0.deinitialize(count: workaround)
      }
    }
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow start)
  public init(
    _unchecked start: UnsafeMutableRawPointer?,
    capacity: Int,
    initializedCount: Int
  ) {
    unsafe _pointer = start
    self.capacity = capacity
    _count = initializedCount
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan: @unchecked Sendable where Element: Sendable & ~Copyable {}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable  {

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _unchecked buffer: UnsafeMutableBufferPointer<Element>,
    initializedCount: Int
  ) {
    unsafe _pointer = .init(buffer.baseAddress)
    capacity = buffer.count
    _count = initializedCount
  }

  /// Unsafely create an OutputSpan over partly-initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime
  /// of the newly-created `OutputSpan`. Its prefix must contain
  /// `initializedCount` initialized instances, followed by uninitialized
  /// memory. The default value of `initializedCount` is 0, representing
  /// the common case of a completely uninitialized `buffer`.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` to be initialized
  ///   - initializedCount: the number of initialized elements
  ///                       at the beginning of `buffer`.
  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    buffer: UnsafeMutableBufferPointer<Element>,
    initializedCount: Int = 0
  ) {
    _precondition(
      ((Int(bitPattern: buffer.baseAddress) &
        (MemoryLayout<Element>.alignment &- 1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    if let baseAddress = buffer.baseAddress {
      _precondition(
        unsafe baseAddress.advanced(by: buffer.count) >= baseAddress,
        "buffer must not overflow"
      )
    }
    _precondition(
      initializedCount >= 0 && initializedCount <= buffer.count,
      "OutputSpan must have a valid count."
    )
    unsafe self.init(_unchecked: buffer, initializedCount: initializedCount)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan {

//  @_alwaysEmitIntoClient
//  @lifetime(borrow buffer)
//  public init(
//    buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>,
//    initializedCount: Int = 0
//  ) {
//    let buffer = unsafe UnsafeMutableBufferPointer(rebasing: buffer)
//    let span = unsafe Self(
//      _unchecked: buffer, initializedCount: initializedCount
//    )
//    self = unsafe _overrideLifetime(span, borrowing: buffer)
//  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: BitwiseCopyable {

//  @_alwaysEmitIntoClient
//  @lifetime(borrow bytes)
//  public init(
//    _initializing bytes: UnsafeMutableRawBufferPointer,
//    initialized: Int = 0
//  ) {
//    _precondition(
//      bytes.baseAddress._isWellAligned(),
//      "baseAddress must be properly aligned to access Element"
//    )
//    let (byteCount, stride) = (bytes.count, MemoryLayout<Element>.stride)
//    let (count, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
//    _precondition(remainder == 0, "Span must contain a whole number of elements")
//    let pointer = bytes.baseAddress
//    let os = OutputSpan(
//      _unchecked: pointer, capacity: count, initialized: initialized
//    )
//    self = unsafe _overrideLifetime(os, borrowing: bytes)
//  }

//  @_alwaysEmitIntoClient
//  @lifetime(borrow pointer)
//  public init(
//    _initializing pointer: UnsafeMutableRawPointer,
//    capacity: Int,
//    initialized: Int = 0
//  ) {
//    _precondition(capacity >= 0, "Capacity must be 0 or greater")
//    let buf = unsafe UnsafeMutableRawBufferPointer(start: pointer, count: capacity)
//    let os = OutputSpan(_initializing: buf, initialized: initialized)
//    self = unsafe _overrideLifetime(os, borrowing: pointer)
//  }

//  @_alwaysEmitIntoClient
//  @lifetime(borrow buffer)
//  public init(
//    _initializing buffer: borrowing Slice<UnsafeMutableRawBufferPointer>,
//    initialized: Int = 0
//  ) {
//    let rebased = unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
//    let os = OutputSpan(_initializing: rebased, initialized: initialized)
//    self = unsafe _overrideLifetime(os, borrowing: buffer)
//  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(_ value: consuming Element) {
    _precondition(_count < capacity, "Output buffer overflow")
    let p = unsafe _start().advanced(by: _count&*MemoryLayout<Element>.stride)
    unsafe p.initializeMemory(as: Element.self, to: value)
    _count &+= 1
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeLast() -> Element? {
    guard _count > 0 else { return nil }
    _count &-= 1
    let p = unsafe _start().advanced(by: _count&*MemoryLayout<Element>.stride)
    return unsafe p.withMemoryRebound(to: Element.self, capacity: 1, { unsafe $0.move() })
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeAll() {
    _ = unsafe _start().withMemoryRebound(to: Element.self, capacity: _count) {
      unsafe $0.deinitialize(count: _count)
    }
    _count = 0
  }
}

//MARK: bulk-update functions
@available(SwiftStdlib 6.2, *)
extension OutputSpan {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(repeating repeatedValue: Element, count: Int) {
    _precondition(
      count <= freeCapacity,
      "destination span cannot contain number of elements requested."
    )
    let offset = _count&*MemoryLayout<Element>.stride
    let p = unsafe _start().advanced(by: offset)
    unsafe p.withMemoryRebound(to: Element.self, capacity: count) {
      unsafe $0.initialize(repeating: repeatedValue, count: count)
    }
    _count &+= count
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append<S>(
    from elements: S
  ) -> S.Iterator where S: Sequence, S.Element == Element {
    var iterator = elements.makeIterator()
    append(from: &iterator)
    return iterator
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    from elements: inout some IteratorProtocol<Element>
  ) {
    while _count < capacity {
      guard let element = elements.next() else { break }
      let p = unsafe _start().advanced(by: _count&*MemoryLayout<Element>.stride)
      unsafe p.initializeMemory(as: Element.self, to: element)
      _count &+= 1
    }
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: some Collection<Element>
  ) {
    let void: Void? = source.withContiguousStorageIfAvailable {
      append(fromContentsOf: unsafe Span(_unsafeElements: $0))
    }
    if void != nil {
      return
    }

    let available = freeCapacity
    let tail = unsafe _start().advanced(by: _count&*MemoryLayout<Element>.stride)
    var (iterator, copied) =
    unsafe tail.withMemoryRebound(to: Element.self, capacity: available) {
      let suffix = unsafe UnsafeMutableBufferPointer(start: $0, count: available)
      return unsafe source._copyContents(initializing: suffix)
    }
    _precondition(
      iterator.next() == nil,
      "destination span cannot contain every element from source."
    )
    assert(_count + copied <= capacity) // invariant check
    _count &+= copied
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: Span<Element>
  ) {
    guard !source.isEmpty else { return }
    _precondition(
      source.count <= freeCapacity,
      "destination span cannot contain every element from source."
    )
    let tail = unsafe _start().advanced(by: _count&*MemoryLayout<Element>.stride)
    _ = unsafe source.withUnsafeBufferPointer {
      unsafe tail.initializeMemory(
        as: Element.self, from: $0.baseAddress!, count: $0.count
      )
    }
    _count += source.count
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: borrowing MutableSpan<Element>
  ) {
    unsafe source.withUnsafeBufferPointer { unsafe append(fromContentsOf: $0) }
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: inout Self
  ) {
    guard !source.isEmpty else { return }
    _precondition(
      source.count <= freeCapacity,
      "buffer cannot contain every element from source."
    )
    let tail = unsafe _start().advanced(by: _count&*MemoryLayout<Element>.stride)
    unsafe source.withUnsafeMutableBufferPointer {
      unsafe tail.moveInitializeMemory(
        as: Element.self, from: $0.baseAddress!, count: $1
      )
      _count &+= $1
      $1 = 0
    }
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: UnsafeMutableBufferPointer<Element>
  ) {
    var source = unsafe OutputSpan(
      buffer: source, initializedCount: source.count
    )
    moveAppend(fromContentsOf: &source)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    unsafe moveAppend(
      fromContentsOf: unsafe UnsafeMutableBufferPointer(rebasing: source)
    )
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: BitwiseCopyable {
// TODO: alternative append() implementations for BitwiseCopyable elements
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var span: Span<Element> {
    @lifetime(borrow self)
    borrowing get {
      let pointer = unsafe _pointer?.assumingMemoryBound(to: Element.self)
      let buffer = unsafe UnsafeBufferPointer(start: pointer, count: _count)
      let span = unsafe Span(_unsafeElements: buffer)
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Element> {
    @lifetime(borrow self)
    mutating get {
      let pointer = unsafe _pointer?.assumingMemoryBound(to: Element.self)
      let buffer = unsafe UnsafeMutableBufferPointer(
        start: pointer, count: _count
      )
      let span = unsafe MutableSpan(_unsafeElements: buffer)
      return unsafe _overrideLifetime(span, mutating: &self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func withUnsafeMutableBufferPointer<E: Error, R: ~Copyable>(
    _ body: (UnsafeMutableBufferPointer<Element>, _ initializedCount: inout Int) throws(E) -> R
  ) throws(E) -> R {
    guard let pointer = unsafe _pointer, capacity > 0 else {
      let buffer = unsafe UnsafeMutableBufferPointer<Element>(start: nil, count: 0)
      var count = 0
      defer { precondition(count == 0, "invalid attempt to grow buffer") }
      return try unsafe body(buffer, &count)
    }
    // bind memory by hand to sidestep alignment concerns
    let binding = Builtin.bindMemory(
      pointer._rawValue, capacity._builtinWordValue, Element.self
    )
    defer { Builtin.rebindMemory(pointer._rawValue, binding) }
    let buffer = unsafe UnsafeMutableBufferPointer<Element>(
      start: .init(pointer._rawValue), count: capacity
    )
    var initializedCount = _count
    defer {
      _precondition(
        initializedCount <= capacity, "invalid attempt to grow buffer"
      )
      _count = initializedCount
    }
    return try unsafe body(buffer, &initializedCount)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @unsafe
  @_alwaysEmitIntoClient
  internal consuming func relinquish() -> UnsafeMutableBufferPointer<Element> {
    let (start, count) = unsafe (self._pointer, self._count)
    discard self
    let typed = unsafe start?.bindMemory(to: Element.self, capacity: count)
    return unsafe UnsafeMutableBufferPointer(start: typed, count: count)
  }

  /// Consume the OutputSpan and return the number of initialized elements.
  ///
  /// Parameters:
  /// - buffer: The buffer being finalized. This must be the same buffer as used to
  ///           initialize the `OutputSpan` instance.
  /// Returns: The number of elements that were initialized.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalize(
    for buffer: UnsafeMutableBufferPointer<Element>
  ) -> Int {
    let (start, capacity, count) = unsafe (_pointer, capacity, count)
    discard self
    unsafe _precondition(
      ((start == UnsafeMutableRawPointer(buffer.baseAddress)) &&
       (capacity == buffer.count)),
      "Expected to finalize for the original buffer."
    )
    return count
  }

  /// Consume the OutputSpan and return the number of initialized elements.
  ///
  /// Parameters:
  /// - buffer: The buffer being finalized. This must be the same buffer as used to
  ///           initialize the `OutputSpan` instance.
  /// Returns: The number of elements that were initialized.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalize(
    for pointer: UnsafeMutablePointer<Element>,
    capacity: Int
  ) -> Int {
    let buffer = unsafe UnsafeMutableBufferPointer(
      start: pointer, count: capacity
    )
    let (start, capacity, count) = unsafe (_pointer, self.capacity, count)
    discard self
    unsafe _precondition(
      ((start == UnsafeMutableRawPointer(buffer.baseAddress)) &&
        (capacity == buffer.count)),
      "Expected to finalize for the original buffer."
    )
    return count
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: BitwiseCopyable {

  /// Consume the OutputSpan and return the number of initialized bytes.
  ///
  /// Parameters:
  /// - buffer: The buffer being finalized. This must be the same buffer as used to
  ///           initialize the `OutputSpan` instance.
  /// Returns: The number of bytes that were initialized.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalize(
    for buffer: UnsafeMutableRawBufferPointer
  ) -> Int {
    let (start, capacity, count) = unsafe (_pointer, capacity, count)
    discard self
    let byteCapacity = capacity &* MemoryLayout<Element>.stride
    unsafe _precondition(
      (start == buffer.baseAddress) && (byteCapacity == buffer.count),
      "Expected to finalize for the original buffer."
    )
    return count &* MemoryLayout<Element>.stride
  }
}
