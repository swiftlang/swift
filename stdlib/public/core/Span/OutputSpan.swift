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
  internal var _initialized: Int = 0

  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  @_alwaysEmitIntoClient
  public var available: Int { capacity &- _initialized }

  @_alwaysEmitIntoClient
  public var count: Int { _initialized }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _initialized == 0 }

  deinit {
    if _initialized > 0 {
      unsafe _start().withMemoryRebound(
        to: Element.self, capacity: _initialized
      ) {
        [ workaround = _initialized ] in
        _ = unsafe $0.deinitialize(count: workaround)
      }
    }
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow start)
  internal init(
    _unchecked start: UnsafeMutableRawPointer?,
    capacity: Int,
    initialized: Int
  ) {
    unsafe _pointer = start
    self.capacity = capacity
    _initialized = initialized
  }
}

@available(SwiftStdlib 6.2, *)
@available(*, unavailable)
extension OutputSpan: Sendable {}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable  {

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  internal init(
    _unchecked buffer: UnsafeMutableBufferPointer<Element>,
    initialized: Int
  ) {
    unsafe _pointer = .init(buffer.baseAddress)
    capacity = buffer.count
    _initialized = initialized
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _initializing buffer: UnsafeMutableBufferPointer<Element>,
    initialized: Int = 0
  ) {
    _precondition(
      buffer.baseAddress._isWellAligned(),
      "baseAddress must be properly aligned to access Element"
    )
    unsafe self.init(_unchecked: buffer, initialized: initialized)
  }

//  @_alwaysEmitIntoClient
//  @lifetime(borrow pointer)
//  public init(
//    _initializing pointer: UnsafeMutablePointer<Element>,
//    capacity: Int,
//    initialized: Int = 0
//  ) {
//    _precondition(capacity >= 0, "Capacity must be 0 or greater")
//    let buf = unsafe UnsafeMutableBufferPointer(start: pointer, count: capacity)
//    let os = OutputSpan(_initializing: buf, initialized: initialized)
//    self = unsafe _overrideLifetime(os, borrowing: pointer)
//  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan {

//  @_alwaysEmitIntoClient
//  @lifetime(borrow buffer)
//  public init(
//    _initializing buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>,
//    initialized: Int = 0
//  ) {
//    let rebased = unsafe UnsafeMutableBufferPointer(rebasing: buffer)
//    let os = OutputSpan(_initializing: rebased, initialized: 0)
//    self = unsafe _overrideLifetime(os, borrowing: buffer)
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
//      ((Int(bitPattern: bytes.baseAddress) &
//        (MemoryLayout<Element>.alignment&-1)) == 0),
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
    _precondition(_initialized < capacity, "Output buffer overflow")
    let p = unsafe _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    unsafe p.initializeMemory(as: Element.self, to: value)
    _initialized &+= 1
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeLast() -> Element? {
    guard _initialized > 0 else { return nil }
    _initialized &-= 1
    let p = unsafe _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    return unsafe p.withMemoryRebound(to: Element.self, capacity: 1, { unsafe $0.move() })
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeAll() {
    _ = unsafe _start().withMemoryRebound(to: Element.self, capacity: _initialized) {
      unsafe $0.deinitialize(count: _initialized)
    }
    _initialized = 0
  }
}

//MARK: bulk-update functions
@available(SwiftStdlib 6.2, *)
extension OutputSpan {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(repeating repeatedValue: Element, count: Int) {
    let available = capacity &- _initialized
    _precondition(
      count <= available,
      "destination span cannot contain number of elements requested."
    )
    let offset = _initialized&*MemoryLayout<Element>.stride
    let p = unsafe _start().advanced(by: offset)
    unsafe p.withMemoryRebound(to: Element.self, capacity: count) {
      unsafe $0.initialize(repeating: repeatedValue, count: count)
    }
    _initialized &+= count
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
    while _initialized < capacity {
      guard let element = elements.next() else { break }
      let p = unsafe _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
      unsafe p.initializeMemory(as: Element.self, to: element)
      _initialized &+= 1
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

    let available = capacity &- _initialized
    let tail = unsafe _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    var (iterator, copied) =
    unsafe tail.withMemoryRebound(to: Element.self, capacity: available) {
      let suffix = unsafe UnsafeMutableBufferPointer(start: $0, count: available)
      return unsafe source._copyContents(initializing: suffix)
    }
    _precondition(
      iterator.next() == nil,
      "destination span cannot contain every element from source."
    )
    assert(_initialized + copied <= capacity) // invariant check
    _initialized &+= copied
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: Span<Element>
  ) {
    guard !source.isEmpty else { return }
    _precondition(
      source.count <= available,
      "destination span cannot contain every element from source."
    )
    let tail = unsafe _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    _ = unsafe source.withUnsafeBufferPointer {
      unsafe tail.initializeMemory(
        as: Element.self, from: $0.baseAddress!, count: $0.count
      )
    }
    _initialized += source.count
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
    fromContentsOf source: consuming Self
  ) {
    guard !source.isEmpty else { return }
    _precondition(
      source.count <= available,
      "buffer cannot contain every element from source."
    )
    let buffer = unsafe source.relinquishBorrowedMemory()
    // we must now deinitialize the returned UMBP
    let tail = unsafe _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    unsafe tail.moveInitializeMemory(
      as: Element.self, from: buffer.baseAddress!, count: buffer.count
    )
    _initialized &+= buffer.count
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: UnsafeMutableBufferPointer<Element>
  ) {
    let source = unsafe OutputSpan(
      _initializing: source, initialized: source.count
    )
    moveAppend(fromContentsOf: source)
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
      let buffer = unsafe UnsafeBufferPointer(start: pointer, count: _initialized)
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
        start: pointer, count: _initialized
      )
      let span = unsafe MutableSpan(_unsafeElements: buffer)
      return unsafe _overrideLifetime(span, mutating: &self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @unsafe
  @_alwaysEmitIntoClient
  public consuming func relinquishBorrowedMemory(
  ) -> UnsafeMutableBufferPointer<Element> {
    let (start, count) = unsafe (self._pointer, self._initialized)
    discard self
    let typed = unsafe start?.bindMemory(to: Element.self, capacity: count)
    return unsafe UnsafeMutableBufferPointer(start: typed, count: count)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: BitwiseCopyable {

  @unsafe
  @_alwaysEmitIntoClient
  public consuming func relinquishBorrowedBytes(
  ) -> UnsafeMutableRawBufferPointer {
    let (start, count) = unsafe (self._pointer, self._initialized)
    discard self
    let byteCount = count&*MemoryLayout<Element>.stride
    return unsafe UnsafeMutableRawBufferPointer(start: start, count: byteCount)
  }
}
