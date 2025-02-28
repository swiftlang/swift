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
    _pointer.unsafelyUnwrapped
  }

  @_alwaysEmitIntoClient
  public var available: Int { capacity &- _initialized }

  @_alwaysEmitIntoClient
  public var count: Int { _initialized }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _initialized == 0 }

  deinit {
    if _initialized > 0 {
      _start().withMemoryRebound(to: Element.self, capacity: _initialized) {
        [ workaround = _initialized ] in
        _ = $0.deinitialize(count: workaround)
      }
    }
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow start)
  internal init(
    _unchecked start: UnsafeMutableRawPointer?,
    capacity: Int,
    initialized: Int
  ) {
    _pointer = start
    self.capacity = capacity
    _initialized = initialized
  }
}

@available(SwiftStdlib 6.2, *)
@available(*, unavailable)
extension OutputSpan: Sendable {}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable  {

  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  internal init(
    _unchecked buffer: UnsafeMutableBufferPointer<Element>,
    initialized: Int
  ) {
    _pointer = .init(buffer.baseAddress)
    capacity = buffer.count
    _initialized = initialized
  }

  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _initializing buffer: UnsafeMutableBufferPointer<Element>,
    initialized: Int = 0
  ) {
    _precondition(
      ((Int(bitPattern: buffer.baseAddress) &
        (MemoryLayout<Element>.alignment&-1)) == 0),
      "baseAddress must be properly aligned to access Element"
    )
    self.init(_unchecked: buffer, initialized: initialized)
  }

//  @_alwaysEmitIntoClient
//  @lifetime(borrow pointer)
//  public init(
//    _initializing pointer: UnsafeMutablePointer<Element>,
//    capacity: Int,
//    initialized: Int = 0
//  ) {
//    _precondition(capacity >= 0, "Capacity must be 0 or greater")
//    let buffer = UnsafeMutableBufferPointer(start: pointer, count: capacity)
//    let os = OutputSpan(_initializing: buffer, initialized: initialized)
//    self = _overrideLifetime(os, borrowing: pointer)
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
//    let rebased = UnsafeMutableBufferPointer(rebasing: buffer)
//    let os = OutputSpan(_initializing: rebased, initialized: 0)
//    self = _overrideLifetime(os, borrowing: buffer)
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
//    self = _overrideLifetime(os, borrowing: bytes)
//  }

//  @_alwaysEmitIntoClient
//  @lifetime(borrow pointer)
//  public init(
//    _initializing pointer: UnsafeMutableRawPointer,
//    capacity: Int,
//    initialized: Int = 0
//  ) {
//    _precondition(capacity >= 0, "Capacity must be 0 or greater")
//    let buffer = UnsafeMutableRawBufferPointer(start: pointer, count: capacity)
//    let os = OutputSpan(_initializing: buffer, initialized: initialized)
//    self = _overrideLifetime(os, borrowing: pointer)
//  }

//  @_alwaysEmitIntoClient
//  @lifetime(borrow buffer)
//  public init(
//    _initializing buffer: borrowing Slice<UnsafeMutableRawBufferPointer>,
//    initialized: Int = 0
//  ) {
//    let rebased = UnsafeMutableRawBufferPointer(rebasing: buffer)
//    let os = OutputSpan(_initializing: rebased, initialized: initialized)
//    self = _overrideLifetime(os, borrowing: buffer)
//  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public mutating func append(_ value: consuming Element) {
    _precondition(_initialized < capacity, "Output buffer overflow")
    let p = _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    p.initializeMemory(as: Element.self, to: value)
    _initialized &+= 1
  }

  @_alwaysEmitIntoClient
  public mutating func deinitializeLastElement() -> Element? {
    guard _initialized > 0 else { return nil }
    _initialized &-= 1
    let p = _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    return p.withMemoryRebound(to: Element.self, capacity: 1, { $0.move() })
  }

  @_alwaysEmitIntoClient
  public mutating func deinitialize() {
    _ = _start().withMemoryRebound(to: Element.self, capacity: _initialized) {
      $0.deinitialize(count: _initialized)
    }
    _initialized = 0
  }
}

//MARK: bulk-update functions
@available(SwiftStdlib 6.2, *)
extension OutputSpan {

  @_alwaysEmitIntoClient
  public mutating func append(repeating repeatedValue: Element, count: Int) {
    let available = capacity &- _initialized
    _precondition(
      count <= available,
      "destination span cannot contain number of elements requested."
    )
    let offset = _initialized&*MemoryLayout<Element>.stride
    let p = _start().advanced(by: offset)
    p.withMemoryRebound(to: Element.self, capacity: count) {
      $0.initialize(repeating: repeatedValue, count: count)
    }
    _initialized &+= count
  }

  @_alwaysEmitIntoClient
  public mutating func append<S>(
    from elements: S
  ) -> S.Iterator where S: Sequence, S.Element == Element {
    var iterator = elements.makeIterator()
    append(from: &iterator)
    return iterator
  }

  @_alwaysEmitIntoClient
  public mutating func append(
    from elements: inout some IteratorProtocol<Element>
  ) {
    while _initialized < capacity {
      guard let element = elements.next() else { break }
      let p = _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
      p.initializeMemory(as: Element.self, to: element)
      _initialized &+= 1
    }
  }

  @_alwaysEmitIntoClient
  public mutating func append(
    fromContentsOf source: some Collection<Element>
  ) {
    let void: Void? = source.withContiguousStorageIfAvailable {
      append(fromContentsOf: Span(_unsafeElements: $0))
    }
    if void != nil {
      return
    }

    let available = capacity &- _initialized
    let tail = _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    var (iterator, copied) =
    tail.withMemoryRebound(to: Element.self, capacity: available) {
      let suffix = UnsafeMutableBufferPointer(start: $0, count: available)
      return source._copyContents(initializing: suffix)
    }
    _precondition(
      iterator.next() == nil,
      "destination span cannot contain every element from source."
    )
    assert(_initialized + copied <= capacity) // invariant check
    _initialized &+= copied
  }

  @_alwaysEmitIntoClient
  public mutating func append(
    fromContentsOf source: Span<Element>
  ) {
    guard !source.isEmpty else { return }
    _precondition(
      source.count <= available,
      "destination span cannot contain every element from source."
    )
    let tail = _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    _ = source.withUnsafeBufferPointer {
      tail.initializeMemory(
        as: Element.self, from: $0.baseAddress!, count: $0.count
      )
    }
    _initialized += source.count
  }

  @_alwaysEmitIntoClient
  public mutating func append(fromContentsOf source: borrowing MutableSpan<Element>) {
    source.withUnsafeBufferPointer { append(fromContentsOf: $0) }
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public mutating func moveAppend(
    fromContentsOf source: consuming Self
  ) {
    guard !source.isEmpty else { return }
    _precondition(
      source.count <= available,
      "buffer cannot contain every element from source."
    )
    let buffer = source.relinquishBorrowedMemory()
    // we must now deinitialize the returned UMBP
    let tail = _start().advanced(by: _initialized&*MemoryLayout<Element>.stride)
    tail.moveInitializeMemory(
      as: Element.self, from: buffer.baseAddress!, count: buffer.count
    )
    _initialized &+= buffer.count
  }

  @_alwaysEmitIntoClient
  public mutating func moveAppend(
    fromContentsOf source: UnsafeMutableBufferPointer<Element>
  ) {
    let source = OutputSpan(_initializing: source, initialized: source.count)
    moveAppend(fromContentsOf: source)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan {

  @_alwaysEmitIntoClient
  public mutating func moveAppend(
    fromContentsOf source: Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    moveAppend(fromContentsOf: UnsafeMutableBufferPointer(rebasing: source))
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
      let pointer = _pointer?.assumingMemoryBound(to: Element.self)
      let buffer = UnsafeBufferPointer(start: pointer, count: _initialized)
      let span = Span(_unsafeElements: buffer)
      return _overrideLifetime(span, borrowing: self)
    }
  }

  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Element> {
    @lifetime(borrow self)
    mutating get {
      let pointer = _pointer?.assumingMemoryBound(to: Element.self)
      let buffer = UnsafeMutableBufferPointer(start: pointer, count: _initialized)
      let span = MutableSpan(_unsafeElements: buffer)
      return _overrideLifetime(span, mutating: &self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public consuming func relinquishBorrowedMemory() -> UnsafeMutableBufferPointer<Element> {
    let (start, count) = (self._pointer, self._initialized)
    discard self
    let typed = start?.bindMemory(to: Element.self, capacity: count)
    return .init(start: typed, count: count)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: BitwiseCopyable {

  @_alwaysEmitIntoClient
  public consuming func relinquishBorrowedBytes() -> UnsafeMutableRawBufferPointer {
    let (start, count) = (self._pointer, self._initialized)
    discard self
    return .init(start: start, count: count&*MemoryLayout<Element>.stride)
  }
}
