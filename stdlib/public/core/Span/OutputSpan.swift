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
  internal var _count: Int

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

  /// Create an OutputSpan with zero capacity
  @_alwaysEmitIntoClient
  @lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    capacity = 0
    _count = 0
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
  @unsafe
  internal func _tail() -> UnsafeMutableRawPointer {
    // NOTE: `_pointer` must be known to be not-nil.
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

  @unsafe
  @_alwaysEmitIntoClient
  @lifetime(borrow buffer)
  public init(
    _uncheckedBuffer buffer: UnsafeMutableBufferPointer<Element>,
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
    initializedCount: Int
  ) {
    _precondition(buffer._isWellAligned(), "Misaligned OutputSpan")
    if let baseAddress = buffer.baseAddress {
      _precondition(
        unsafe baseAddress.advanced(by: buffer.count) >= baseAddress,
        "Buffer must not wrap around the address space"
      )
    }
    _precondition(
      0 <= initializedCount && initializedCount <= buffer.count,
      "OutputSpan count is not within capacity"
    )
    unsafe self.init(
      _uncheckedBuffer: buffer, initializedCount: initializedCount
    )
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan {

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
    buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>,
    initializedCount: Int
  ) {
    let rebased = unsafe UnsafeMutableBufferPointer(rebasing: buffer)
    let os = unsafe OutputSpan(
      buffer: rebased, initializedCount: initializedCount
    )
    self = unsafe _overrideLifetime(os, borrowing: buffer)
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {

  public typealias Index = Int

  @_alwaysEmitIntoClient
  public var indices: Range<Index> {
    unsafe Range(_uncheckedBounds: (0, _count))
  }

  /// Accesses the element at the specified position.
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

  /// Accesses the element at the specified position.
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
  public mutating func removeLast() -> Element {
    _precondition(!isEmpty, "OutputSpan underflow")
    _count &-= 1
    return unsafe _tail().withMemoryRebound(to: Element.self, capacity: 1) {
      unsafe $0.move()
    }
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func removeLast(_ k: Int) {
    _precondition(k >= 0, "Can't remove a negative number of elements")
    _precondition(k <= _count, "OutputSpan underflow")
    _count &-= k
    unsafe _tail().withMemoryRebound(to: Element.self, capacity: k) {
      _ = unsafe $0.deinitialize(count: k)
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

//MARK: bulk-append functions
@available(SwiftStdlib 6.2, *)
extension OutputSpan {

  /// Initialize this span's suffix to the repetitions of the given value.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(repeating repeatedValue: Element, count: Int) {
    _precondition(count <= freeCapacity, "OutputSpan capacity overflow")
    unsafe _tail().initializeMemory(
      as: Element.self, repeating: repeatedValue, count: count
    )
    _count &+= count
  }

  /// Returns true if the iterator has filled all the free capacity in the span.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  @discardableResult
  public mutating func append(
    from elements: inout some IteratorProtocol<Element>
  ) -> Bool {
    // FIXME: It may be best to delay this API until
    //        we have designed a chunking IteratorProtocol
    var p = unsafe _tail()
    while _count < capacity {
      guard let element = elements.next() else { return false }
      unsafe p.initializeMemory(as: Element.self, to: element)
      unsafe p = p.advanced(by: MemoryLayout<Element>.stride)
      _count &+= 1
    }
    return true
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    contentsOf source: consuming some Sequence<Element>
  ) {
    let done: Void? = source.withContiguousStorageIfAvailable {
      unsafe append(contentsOf: $0)
    }
    if done != nil {
      return
    }

    let freeCapacity = freeCapacity
    let tail = unsafe _tail()._rawValue
    // rebind memory manually to get around closure issues
    // see https://github.com/swiftlang/swift/issues/81525
    let binding = Builtin.bindMemory(
      tail, freeCapacity._builtinWordValue, Element.self
    )
    defer { Builtin.rebindMemory(tail, binding) }
    let suffix = unsafe UnsafeMutableBufferPointer<Element>(
      start: .init(tail), count: freeCapacity
    )
    var (iterator, copied) = unsafe source._copyContents(initializing: suffix)

    _precondition(iterator.next() == nil, "OutputSpan capacity overflow")
    _precondition(_count + copied <= capacity, "Invalid Sequence._copyContents")
    _count &+= copied
  }

  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    contentsOf source: UnsafeBufferPointer<Element>
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
    copying source: borrowing Span<Element>
  ) {
    unsafe source.withUnsafeBufferPointer { unsafe append(contentsOf: $0) }
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {
  /// Append to this output span by moving elements out of the given output
  /// span, leaving it empty. The elements are appended in the order they appear
  /// in the source.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    moving source: inout Self
  ) {
    guard !source.isEmpty else { return }
    unsafe source.withUnsafeMutableBufferPointer {
      unsafe self.append(consuming: $0.extracting(..<$1))
      $1 = 0
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan where Element: ~Copyable {
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    consuming source: UnsafeMutableBufferPointer<Element>
  ) {
    guard !source.isEmpty else { return }
    _precondition(source.count <= freeCapacity, "OutputSpan capacity overflow")
    unsafe _tail().moveInitializeMemory(
      as: Element.self, from: source.baseAddress!, count: source.count
    )
    _count += source.count
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan {
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func append(
    consuming source: Slice<UnsafeMutableBufferPointer<Element>>
  ) {
    unsafe append(consuming: UnsafeMutableBufferPointer(rebasing: source))
  }
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
  /// Call the given closure with the unsafe buffer pointer addressed by this
  /// OutputSpan and a mutable reference to its count of initialized elements.
  ///
  /// This method provides a way to process or populate an `OutputSpan` using
  /// unsafe operations, such as dispatching to code written in legacy
  /// (memory-unsafe) languages.
  ///
  /// The supplied closure may process the buffer in any way it wants; however,
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
  /// This function cannot verify these two invariants, and therefore
  /// this is an unsafe operation. Violating the invariants of `OutputSpan`
  /// may result in undefined behavior.
  @_alwaysEmitIntoClient
  @lifetime(self: copy self)
  public mutating func withUnsafeMutableBufferPointer<E: Error, R: ~Copyable>(
    _ body: (
      UnsafeMutableBufferPointer<Element>,
      _ initializedCount: inout Int
    ) throws(E) -> R
  ) throws(E) -> R {
    guard let start = unsafe _pointer, capacity > 0 else {
      let buffer = UnsafeMutableBufferPointer<Element>(_empty: ())
      var initializedCount = 0
      defer {
        _precondition(initializedCount == 0, "OutputSpan capacity overflow")
      }
      return unsafe try body(buffer, &initializedCount)
    }
    // bind memory by hand to sidestep alignment concerns
    let binding = Builtin.bindMemory(
      start._rawValue, capacity._builtinWordValue, Element.self
    )
    defer { Builtin.rebindMemory(start._rawValue, binding) }
    let buffer = unsafe UnsafeMutableBufferPointer<Element>(
      _uncheckedStart: .init(start._rawValue), count: capacity
    )
    var initializedCount = self._count
    defer {
      _precondition(
        0 <= initializedCount && initializedCount <= capacity,
        "OutputSpan capacity overflow"
      )
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
  /// This method should be invoked in the scope where the `OutputSpan` was
  /// created, when it is time to commit the contents of the updated buffer
  /// back into the construct being initialized.
  ///
  /// The context that created the output span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer we expect the `OutputSpan` to reference.
  ///      This must be the same region of memory passed to
  ///      the `OutputSpan` initializer.
  /// - Returns: The number of initialized elements in the same buffer, as
  ///      tracked by the consumed `OutputSpan` instance.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalize(
    for buffer: UnsafeMutableBufferPointer<Element>
  ) -> Int {
    _precondition(
      unsafe UnsafeMutableRawPointer(buffer.baseAddress) == self._pointer
      && buffer.count == self.capacity,
      "OutputSpan identity mismatch"
    )
    let count = self._count
    discard self
    return count
  }
}

@available(SwiftStdlib 6.2, *)
extension OutputSpan {
  /// Consume the output span (relinquishing its control over the buffer it is
  /// addressing), and return the number of initialized elements in it.
  ///
  /// This method should be invoked in the scope where the `OutputSpan` was
  /// created, when it is time to commit the contents of the updated buffer
  /// back into the construct being initialized.
  ///
  /// The context that created the output span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer we expect the `OutputSpan` to reference.
  ///      This must be the same region of memory passed to
  ///      the `OutputSpan` initializer.
  /// - Returns: The number of initialized elements in the same buffer, as
  ///      tracked by the consumed `OutputSpan` instance.
  @unsafe
  @_alwaysEmitIntoClient
  public consuming func finalize(
    for buffer: Slice<UnsafeMutableBufferPointer<Element>>
  ) -> Int {
    unsafe finalize(for: UnsafeMutableBufferPointer(rebasing: buffer))
  }
}
