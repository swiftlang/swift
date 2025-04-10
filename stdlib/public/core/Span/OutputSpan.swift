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

// `OutputSpan` is a reference to a contiguous region of memory that starts with
// some number of initialized `Element` instances followed by uninitialized
// memory. It provides operations to access the items it stores, as well as to
// add new elements and to remove existing ones.
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
  @inlinable
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
    _uncheckedStart start: UnsafeMutableRawPointer?,
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
extension OutputSpan where Element: ~Copyable {
  @_alwaysEmitIntoClient
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  @_alwaysEmitIntoClient
  @_transparent
  internal func _tail() -> UnsafeMutableRawPointer {
    unsafe _start().advanced(by: _count &* MemoryLayout<Element>.stride)
  }

  @_alwaysEmitIntoClient
  public var freeCapacity: Int { capacity &- _count }

  @_alwaysEmitIntoClient
  public var count: Int { _count }

  @_alwaysEmitIntoClient
  public var isEmpty: Bool { _count == 0 }

  @_alwaysEmitIntoClient
  public var isFull: Bool { _count == capacity }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable  {
  @_alwaysEmitIntoClient
  @lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    capacity = 0
    _count = 0
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _uncheckedBuffer buffer: UnsafeMutableBufferPointer<Element>,
    initializedCount: Int
  ) {
    unsafe _pointer = .init(buffer.baseAddress)
    capacity = buffer.count
    _count = count
  }

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    buffer buffer: UnsafeMutableBufferPointer<Element>,
    initializedCount: Int = 0
  ) {
    _precondition(buffer._isWellAligned(), "Misaligned OutputSpan")
    _precondition(
      buffer.baseAddress?.advanced(by: buffer.count) >= buffer.baseAddress,
      "OutputSpan must not wrap around the address space"
    )
    _precondition(
      initializedCount >= 0 && initializedCount <= buffer.count,
      "OutputSpan count is outside its capacity"
    )
    unsafe self.init(_uncheckedBuffer: buffer, initializedCount: initializedCount)
  }

  // FIXME: Delete this
 // @_alwaysEmitIntoClient
 // @lifetime(borrow pointer)
 // public init(
 //   _start pointer: UnsafeMutablePointer<Element>,
 //   capacity: Int,
 //   initializedCount: Int = 0
 // ) {
 //   _precondition(capacity >= 0, "OutputSpan capacity cannot be negative")
 //   let buf = unsafe UnsafeMutableBufferPointer(start: pointer, count: capacity)
 //   let os = unsafe OutputSpan(_buffer: buf, initializedCount: initializedCount)
 //   self = unsafe _overrideLifetime(os, borrowing: pointer)
 // }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan {
  // FIXME: Delete this
  // @_alwaysEmitIntoClient
  // @lifetime(borrow buffer)
  // public init(
  //   _buffer buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>,
  //   initializedCount: Int = 0
  // ) {
  //   let rebased = unsafe UnsafeMutableBufferPointer(rebasing: buffer)
  //   let os = unsafe OutputSpan(
  //     _buffer: rebased, initializedCount: initializedCount)
  //   self = unsafe _overrideLifetime(os, borrowing: buffer)
  // }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: BitwiseCopyable {
  // FIXME: BitwiseCopyable alone does not make this safe. Remove it.
  // @_alwaysEmitIntoClient
  // @lifetime(borrow bytes)
  // public init(
  //   _bytes bytes: UnsafeMutableRawBufferPointer,
  //   initializedCount: Int = 0 // FIXME: Beware, unit mismatch
  // ) {
  //   guard let start = bytes.baseAddress else {
  //     _precondition(
  //       initializedCount == 0, "OutputSpan count is outside its capacity")
  //     self.init()
  //     return
  //   }
  //   _precondition(
  //     start == start.alignedDown(for: Element.self), "Misaligned OutputSpan")
  //   let (byteCount, stride) = (bytes.count, MemoryLayout<Element>.stride)
  //   let (capacity, remainder) = byteCount.quotientAndRemainder(dividingBy: stride)
  //   _precondition(remainder == 0, "OutputSpan must contain a whole number of elements")
  //   _precondition(
  //     initializedCount >= 0 && initializedCount <= capacity,
  //     "OutputSpan count is outside its capacity")
  //   let span = unsafe OutputSpan(
  //     _uncheckedStart: start,
  //     capacity: capacity,
  //     initializedCount: initializedCount)
  //   self = unsafe _overrideLifetime(span, borrowing: bytes)
  // }

  // FIXME: Remove this.
  // @_alwaysEmitIntoClient
  // @lifetime(borrow buffer)
  // public init(
  //   _bytes buffer: borrowing Slice<UnsafeMutableRawBufferPointer>,
  //   initializedCount: Int = 0 // FIXME: Beware, unit mismatch
  // ) {
  //   let rebased = unsafe UnsafeMutableRawBufferPointer(rebasing: buffer)
  //   let os = unsafe OutputSpan(_bytes: rebased, initializedCount: initializedCount)
  //   self = unsafe _overrideLifetime(os, borrowing: buffer)
  // }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(_ value: consuming Element) {
    _precondition(_count < capacity, "OutputSpan capacity overflow")
    unsafe _tail().initializeMemory(as: Element.self, to: value)
    _count &+= 1
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeLast() -> Element? {
    guard _count > 0 else { return nil }
    _count &-= 1
    return unsafe _tail().withMemoryRebound(to: Element.self, capacity: 1) {
      unsafe $0.move()
    }
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
    _precondition(count <= freeCapacity, "OutputSpan capacity overflow")
    unsafe _tail().withMemoryRebound(to: Element.self, capacity: count) {
      unsafe $0.initialize(repeating: repeatedValue, count: count)
    }
    _count &+= count
  }


  /// Returns `true` if it has reached the end of the iterator without filling
  /// up all free capacity in the target span.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  @discardableResult
  public mutating func append(
    fromContentsOf elements: inout some IteratorProtocol<Element>
  ) -> Bool {
    // FIXME: It may be best to delay this API until we've a chunking IteratorProtocol
    var p = unsafe _tail()
    while _count < capacity {
      guard let element = elements.next() else { return true }
      unsafe p.initializeMemory(as: Element.self, to: element)
      p += MemoryLayout<Element>.stride
      _count &+= 1
    }
    return false
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: some Sequence<Element>
  ) {
    let done: Void? = source.withContiguousStorageIfAvailable {
      unsafe append(fromContentsOf: $0)
    }
    if done != nil {
      return
    }

    let freeCapacity = freeCapacity
    var (iterator, copied) = unsafe _tail().withMemoryRebound(
      to: Element.self, capacity: freeCapacity
    ) {
      let suffix = unsafe UnsafeMutableBufferPointer(start: $0, count: freeCapacity)
      return unsafe source._copyContents(initializing: suffix)
    }
    _precondition(iterator.next() == nil, "OutputSpan capacity overflow")
    _precondition(_count + copied <= capacity, "Invalid Sequence._copyContents")
    _count &+= copied
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: UnsafeBufferPointer<Element>
  ) {
    guard !source.isEmpty else { return }
    _precondition(source.count <= freeCapacity, "OutputSpan capacity overflow")
    unsafe _tail().initializeMemory(
      as: Element.self, from: source.baseAddress!, count: source.count)
    _count += source.count
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    fromContentsOf source: Span<Element>
  ) {
    unsafe source.withUnsafeBufferPointer { self.append(fromContentsOf: $0) }
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
  /// Append to this output span by moving elements out of the given output
  /// span, leaving it empty. The elements are appended in the order they appear
  /// in the source.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: inout Self
  ) {
    guard !source.isEmpty else { return }
    unsafe source.withUnsafeMutableBufferPointer { buffer, count in
      unsafe self.moveAppend(fromContentsOf: buffer.extracting(..<count))
      count = 0
    }
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func moveAppend(
    fromContentsOf source: UnsafeMutableBufferPointer<Element>
  ) {
    guard !source.isEmpty else { return }
    _precondition(source.count <= freeCapacity, "OutputSpan capacity overflow")
    unsafe _tail().moveInitializeMemory(
      as: Element.self, from: source.baseAddress!, count: source.count)
    self._count += count
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
      fromContentsOf: UnsafeMutableBufferPointer(rebasing: source))
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  @_alwaysEmitIntoClient
  public var span: Span<Element> {
    @lifetime(borrow self)
    borrowing get {
      // FIXME: Do we expect OutputSpan's memory to be bound or not?
      let pointer = unsafe _pointer?.assumingMemoryBound(to: Element.self)
      let buffer = unsafe UnsafeBufferPointer(start: pointer, count: _count)
      let span = unsafe Span(_unsafeElements: buffer)
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Element> {
    @lifetime(&self)
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
  /// Call the given function with the unsafe buffer pointer addressed by this
  /// `OutputSpan` and a mutable reference to its count of initialized elements.
  /// This method provides a way for Swift code to process or populate an
  /// `OutputSpan` using unsafe operations; for example, it allows dispatching
  /// to code written in legacy (memory-unsafe) languages.
  ///
  /// The supplied function may process the buffer in any way it wants; however,
  /// when it finishes (whether by returning or throwing), it must leave the
  /// buffer in a state that satisfies the invariants of the output span:
  ///
  /// 1. The inout integer passed in as the second argument must be the exact
  ///     number of initialized items in the buffer passed in as the first
  ///     argument.
  /// 2. These initialized elements must be located in a single contiguous
  ///     region starting at the beginning of the buffer. The rest of the buffer
  ///     must hold uninitialized memory.
  ///
  /// This operation cannot verify that this is actually the case, and that
  /// makes this a fundamentally unsafe operation. Violating the output span
  /// invariants results in undefined behavior.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func withUnsafeMutableBufferPointer<E: Error, R: ~Copyable>(
    _ body: (
      UnsafeMutableBufferPointer<Element>,
      _ initializedCount: inout Int
    ) throws(E) -> R
  ) throws(E) -> R {
    guard let start = unsafe _pointer else {
      let buffer = unsafe UnsafeMutableBufferPointer<Element>(
        start: nil, count: 0)
      var count = 0
      let result = unsafe try body(buffer, &count)
      _precondition(count == 0, "OutputSpan count outside its capacity")
      return result
    }
    // bind memory by hand to sidestep alignment concerns
    let binding = Builtin.bindMemory(
      start._rawValue, capacity._builtinWordValue, Element.self)
    defer { Builtin.rebindMemory(start._rawValue, binding) }
    let buffer = unsafe UnsafeMutableBufferPointer<Element>(start: start, count: capacity)
    var initializedCount = self._count
    defer {
      _precondition(
        initializedCount >= 0 && initializedCount <= self.capacity,
          "OutputSpan count outside its capacity")
      self._count = initializedCount
    }
    return unsafe try body(buffer, &initializedCount)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {
  /// Consume the output span (relinquishing its control over the buffer it is
  /// addressing), and return the number of initialized elements in it.
  ///
  /// This method is designed to be invoked in the same context that created the
  /// output span, when it is time to commit the contents of the updated buffer
  /// back into the construct that it came from.
  ///
  /// The context that created the output span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer that the `OutputSpan` is expected to
  ///      address. This must be the same buffer as the one used to originally
  ///      initialize the `OutputSpan` instance.
  /// - Returns: The number of initialized elements in the same buffer, as
  ///      tracked by the consumed `OutputSpan` instance.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalize(
    for buffer: UnsafeMutableBufferPointer<Element>
  ) -> Int {
    _precondition(
      unsafe buffer.baseAddress == self._pointer
      && buffer.count == self.capacity,
      "OutputSpan identity mismatch")
    let count = self._count
    discard self
    return count
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: BitwiseCopyable {
  // FIXME: Consider removing this; OutputRawSpan should be the canonical way to do it.

  /// Consume the output span (relinquishing its control over the buffer it is
  /// addressing), and return the number of initialized bytes in it.
  ///
  /// This method is designed to be invoked in the same context that created the
  /// output span, when it is time to commit the contents of the updated buffer
  /// back into the construct that it came from.
  ///
  /// The context that created the output span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer that we the `OutputSpan` is expected to
  ///      relinquish using. This must be the same buffer as the one used to
  ///      originally initialize the `OutputSpan` instance.
  /// - Returns: The number of initialized bytes in the same buffer, as
  ///      tracked by the consumed `OutputSpan` instance.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalizeBytes(
    for buffer: UnsafeMutableRawBufferPointer
  ) -> Int {
    _precondition(
      unsafe buffer.baseAddress == self._pointer
      && buffer.count == self.capacity,
      "OutputSpan identity mismatch")
    let count = self._count
    discard self
    return count &* MemoryLayout<Element>.stride
  }
}
