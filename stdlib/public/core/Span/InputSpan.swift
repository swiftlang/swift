//===--- InputSpan.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// `InputSpan` is a reference to a contiguous region of memory that starts
/// with uninitialized memory, followed by some number of initialized `Element`
/// instances. It provides operations to access the items it stores,
/// as well as to consume existing elements and to prepend new ones.
@safe
@frozen
@available(SwiftStdlib 6.5, *)
public struct InputSpan<Element: ~Copyable>: ~Copyable, ~Escapable {
  @usableFromInline
  internal let _pointer: UnsafeMutableRawPointer?

  /// The total number of elements that this input span can contain.
  @usableFromInline
  internal let _capacity: Int

  @usableFromInline
  internal var _count: Int

  @export(implementation)
  deinit {
    if count > 0 {
      unsafe _unsafeRawAddressOfSlot(uncheckedOffset: 0).withMemoryRebound(
        to: Element.self, capacity: count
      ) {
        _ = unsafe $0.deinitialize(count: count)
      }
    }
  }

  /// Create an InputSpan with zero capacity.
  @export(implementation)
  @_lifetime(immortal)
  public init() {
    unsafe _pointer = nil
    _capacity = 0
    _count = 0
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan: @unchecked Sendable where Element: Sendable & ~Copyable {}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {
  /// Returns the address of the start of the span's underlying buffer
  ///
  /// NOTE: `_pointer` must be known to be not-nil.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  internal func _start() -> UnsafeMutableRawPointer {
    unsafe _pointer._unsafelyUnwrappedUnchecked
  }

  /// Returns the address of the slot at `offset` positions from the first
  /// initialized element. Negative offsets address the uninitialized
  /// region that precedes the initialized elements.
  ///
  /// NOTE: `_pointer` must be known to be not-nil.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  internal func _unsafeRawAddressOfSlot(
    uncheckedOffset offset: Int
  ) -> UnsafeMutableRawPointer {
    let slot = capacity &- count &+ offset
    return unsafe _start().advanced(by: slot &* MemoryLayout<Element>.stride)
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  internal func _unsafeAddressOfElement(
    unchecked index: Index
  ) -> UnsafeMutablePointer<Element> {
    unsafe _unsafeRawAddressOfSlot(
      uncheckedOffset: index
    ).assumingMemoryBound(to: Element.self)
  }

  /// The address of the first initialized element, or `nil` if this span
  /// does not reference any memory.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @unsafe
  internal func _initializedStart() -> UnsafeMutableRawPointer? {
    let offset = (capacity &- count) &* MemoryLayout<Element>.stride
    return unsafe _pointer?.advanced(by: offset)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {
  /// The number of initialized elements in this span.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_semantics("fixed_storage.get_count")
  public var count: Int { _assumeNonNegative(_count) }

  @available(SwiftStdlib 6.5, *)
  @export(implementation) @inline(always)
  public var capacity: Int { _assumeNonNegative(_capacity) }

  /// The number of additional elements that can be added to this span.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var freeCapacity: Int {
    _assumeNonNegative(capacity &- count)
  }

  /// A Boolean value indicating whether the span is empty.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var isEmpty: Bool { count == 0 }

  /// A Boolean value indicating whether the span is full.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var isFull: Bool { count == capacity }

  /// Unsafely accesses the number of initialized elements in this span.
  ///
  /// Writing through the returned reference changes `count` without
  /// initializing or deinitializing any element. The caller must keep the
  /// span consistent: the new count must be between 0 and `capacity`, and
  /// exactly that many slots at the end of the buffer
  /// must hold initialized elements.
  ///
  /// The returned reference represents a mutation of `self`, and `self`
  /// cannot be used until the reference is no longer in use.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  public var _unsafeStoredCount: MutableRef<Int> {
    @_lifetime(&self)
    @_transparent
    mutating get {
      MutableRef(&_count)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {

  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  internal init(
    _uncheckedBuffer buffer: UnsafeMutableBufferPointer<Element>,
    initializedCount: Int
  ) {
    unsafe _pointer = .init(buffer.baseAddress)
    _capacity = buffer.count
    _count = initializedCount
  }

  /// Unsafely create an InputSpan over partly-initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime
  /// of the newly-created `InputSpan`. Its suffix must contain
  /// `initializedCount` initialized instances, preceded by uninitialized
  /// memory.
  ///
  /// - Parameters:
  ///   - buffer: an `UnsafeMutableBufferPointer` with some number of
  ///       initialized elements, all compacted into the end of the buffer.
  ///   - initializedCount: the number of initialized elements
  ///       at the end of `buffer`.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(
    buffer: UnsafeMutableBufferPointer<Element>,
    initializedCount: Int
  ) {
    if let baseAddress = buffer.baseAddress {
      _precondition(
        unsafe baseAddress <= baseAddress.advanced(by: buffer.count),
        "Buffer must not wrap around the address space"
      )
    } else {
      _precondition(buffer.count == 0)
    }
    _precondition(
      UInt(bitPattern: initializedCount) <= UInt(bitPattern: buffer.count),
      "InputSpan count is invalid or greater than capacity."
    )
    unsafe self.init(
      _uncheckedBuffer: buffer, initializedCount: initializedCount
    )
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan {

  /// Unsafely create an InputSpan over partly-initialized memory.
  ///
  /// The memory in `buffer` must remain valid throughout the lifetime
  /// of the newly-created `InputSpan`. Its suffix must contain
  /// `initializedCount` initialized instances, preceded by uninitialized
  /// memory.
  ///
  /// - Parameters:
  ///   - buffer: a slice of an `UnsafeMutableBufferPointer` with some number
  ///       of initialized elements, all compacted into the end of the slice.
  ///   - initializedCount: the number of initialized elements
  ///       at the end of `buffer`.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(borrow buffer)
  public init(
    buffer: borrowing Slice<UnsafeMutableBufferPointer<Element>>,
    initializedCount: Int
  ) {
    let rebased = unsafe UnsafeMutableBufferPointer(rebasing: buffer)
    let os = unsafe InputSpan(
      buffer: rebased, initializedCount: initializedCount
    )
    self = unsafe _overrideLifetime(os, borrowing: buffer)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {
  /// The type that represents an initialized index in an `InputSpan`.
  @available(SwiftStdlib 6.5, *)
  public typealias Index = Int

  /// The range of initialized indices for this `InputSpan`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public var indices: Range<Index> {
    unsafe Range(_uncheckedBounds: (0, count))
  }

  // SILOptimizer looks for fixed_storage.check_index semantics
  // for bounds check optimizations.
  @available(SwiftStdlib 6.5, *)
  @_semantics("fixed_storage.check_index")
  @inline(__always)
  @export(implementation)
  internal func _checkIndex(_ index: Index) {
    _precondition(indices.contains(index), "index out of bounds")
  }

  /// Accesses the element at the specified index.
  ///
  /// - Parameter index: A valid index into this span.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public subscript(_ index: Index) -> Element {
    @_transparent
    borrow {
      _checkIndex(index)
      return unsafe self[unchecked: index]
    }
    @_transparent
    @_lifetime(self: copy self)
    mutate {
      _checkIndex(index)
      return unsafe &(self[unchecked: index])
    }
  }

  /// Accesses the element at the specified index.
  ///
  /// This subscript does not validate `index`; this is an unsafe operation.
  ///
  /// - Parameter index: A valid index into this span.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  public subscript(unchecked index: Index) -> Element {
    @_unsafeSelfDependentResult
    borrow {
      unsafe UnsafePointer(_unsafeAddressOfElement(unchecked: index)).pointee
    }
    @_unsafeSelfDependentResult
    @_lifetime(self: copy self)
    mutate {
      unsafe &(_unsafeAddressOfElement(unchecked: index).pointee)
    }
  }

  /// Exchange the elements at the two given indices.
  ///
  /// - Parameter i: A valid index into this span.
  /// - Parameter j: A valid index into this span.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func swapAt(_ i: Index, _ j: Index) {
    _precondition(indices.contains(i))
    _precondition(indices.contains(j))
    unsafe swapAt(unchecked: i, unchecked: j)
  }

  /// Exchange the elements at the two given indices.
  ///
  /// This function does not validate `i` or `j`; this is an unsafe operation.
  ///
  /// - Parameter i: A valid index into this span.
  /// - Parameter j: A valid index into this span.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func swapAt(unchecked i: Index, unchecked j: Index) {
    guard i != j else { return }
    let pi = unsafe _unsafeAddressOfElement(unchecked: i)
    let pj = unsafe _unsafeAddressOfElement(unchecked: j)
    let temporary = unsafe pi.move()
    unsafe pi.initialize(to: pj.move())
    unsafe pj.initialize(to: consume temporary)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {
  /// Prepend a single element to this span.
  ///
  /// - Parameter value: The element to prepend.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func prepend(_ value: consuming Element) {
    _precondition(count < capacity, "InputSpan capacity overflow")
    unsafe _unsafeRawAddressOfSlot(
      uncheckedOffset: -1
    ).initializeMemory(as: Element.self, to: value)
    _count &+= 1
  }

  /// Remove the first initialized element from this span.
  ///
  /// Returns the first element. The `InputSpan` must not be empty.
  ///
  /// - Returns: The removed element.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func removeFirst() -> Element {
    _precondition(!isEmpty, "InputSpan underflow")
    let element = unsafe _unsafeRawAddressOfSlot(
      uncheckedOffset: 0
    ).withMemoryRebound(to: Element.self, capacity: 1) {
      unsafe $0.move()
    }
    _count &-= 1
    return element
  }

  /// Remove the first n elements of this span, returning the memory
  /// they occupy to the uninitialized state.
  ///
  /// - Parameter n: The number of elements to remove.
  ///     `n` must not be negative or greater than `count`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func removeFirst(_ n: Int) {
    _precondition(n >= 0, "Can't remove a negative number of elements")
    _precondition(n <= count, "InputSpan underflow")
    guard n > 0 else { return }
    unsafe _unsafeRawAddressOfSlot(uncheckedOffset: 0).withMemoryRebound(
      to: Element.self, capacity: n
    ) {
      _ = unsafe $0.deinitialize(count: n)
    }
    _count &-= n
  }

  /// Removes and returns the first element of this span, if it exists.
  ///
  /// - Returns: The first element of the span if the span isn't empty;
  ///    otherwise, `nil`.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func popFirst() -> Element? {
    guard !isEmpty else { return nil }
    return removeFirst()
  }

  /// Remove all this span's elements and return its memory
  /// to the uninitialized state.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func removeAll() {
    guard count > 0 else { return }
    unsafe _unsafeRawAddressOfSlot(uncheckedOffset: 0).withMemoryRebound(
      to: Element.self, capacity: count
    ) {
      _ = unsafe $0.deinitialize(count: count)
    }
    _count = 0
  }
}

//MARK: bulk-prepend functions
@available(SwiftStdlib 6.5, *)
extension InputSpan {

  /// Repeatedly prepend an element to this span.
  ///
  /// - Parameters:
  ///   - repeatedValue: The element to prepend repeatedly.
  ///   - count: The number of times to prepend `repeatedValue`.
  ///       `count` must not exceed `freeCapacity`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(self: copy self)
  public mutating func prepend(repeating repeatedValue: Element, count: Int) {
    _precondition(count >= 0, "Can't prepend a negative number of values")
    _precondition(count <= freeCapacity, "InputSpan capacity overflow")
    guard count > 0 else { return }
    unsafe _unsafeRawAddressOfSlot(uncheckedOffset: -count).initializeMemory(
      as: Element.self, repeating: repeatedValue, count: count
    )
    _count &+= count
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {
  /// Borrow the underlying initialized memory for read-only access.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var span: Span<Element> {
    @_lifetime(borrow self)
    borrowing get {
      let span = unsafe Span<Element>(
        _unchecked: _initializedStart(), count: count
      )
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  /// Exclusively borrow the underlying initialized memory for mutation.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  public var mutableSpan: MutableSpan<Element> {
    @_lifetime(&self)
    mutating get {
      let span = unsafe MutableSpan<Element>(
        _unchecked: _initializedStart(), count: count
      )
      return unsafe _overrideLifetime(span, mutating: &self)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {
  /// Call the given closure with the unsafe buffer pointer addressed by this
  /// InputSpan and a mutable reference to its count of initialized elements.
  ///
  /// This method provides a way to process or consume the contents of an
  /// `InputSpan` using unsafe operations, such as dispatching to code written
  /// in legacy (memory-unsafe) languages.
  ///
  /// The supplied closure may process the buffer in any way it wants; however,
  /// when it finishes (whether by returning or throwing), it must leave the
  /// buffer in a state that satisfies the invariants of the input span:
  ///
  /// 1. The inout integer passed in as the second argument must be the exact
  ///     number of initialized items in the buffer passed in as the first
  ///     argument.
  /// 2. These initialized elements must be located in a single contiguous
  ///     region located at the end of the buffer. The initialized region must
  ///     be preceded by uninitialized memory.
  ///
  /// This function cannot verify these two invariants, and therefore
  /// this is an unsafe operation. Violating the invariants of `InputSpan`
  /// may result in undefined behavior.
  ///
  /// - Parameter body: A closure that can read from and write to the
  ///     buffer and update the initialized count.
  /// - Returns: The return value of the `body` closure.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_transparent
  @_lifetime(self: copy self)
  @unsafe
  public mutating func withUnsafeMutableBufferPointer<E: Error, R: ~Copyable>(
    _ body: (
      UnsafeMutableBufferPointer<Element>,
      _ initializedCount: inout Int
    ) throws(E) -> R
  ) throws(E) -> R {
    let bytes = unsafe UnsafeMutableRawBufferPointer(
      start: _pointer, count: capacity &* MemoryLayout<Element>.stride
    )
    var initializedCount = count
    defer {
      _precondition(
        UInt(bitPattern: initializedCount) <= UInt(bitPattern: capacity),
        "InputSpan capacity overflow"
      )
      self._count = initializedCount
    }
    return try unsafe bytes.withMemoryRebound(to: Element.self) {
      buffer throws(E) -> R in
      try unsafe body(buffer, &initializedCount)
    }
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {
  /// Consume the input span and return the number of initialized elements
  /// remaining at the end of the underlying memory region.
  ///
  /// This method should be invoked in the scope where the `InputSpan` was
  /// created, when it is time to commit the contents of the updated buffer
  /// back into the construct that was accessed.
  ///
  /// The context that created the input span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer we expect the `InputSpan` to reference.
  ///      This must be the same region of memory passed to
  ///      the `InputSpan` initializer.
  /// - Returns: The number of initialized elements remaining at the end of
  ///      the same buffer, as tracked by the consumed `InputSpan` instance.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  public consuming func finalize(
    for buffer: UnsafeMutableBufferPointer<Element>
  ) -> Int {
    _precondition(
      unsafe UnsafeMutableRawPointer(buffer.baseAddress) == _pointer
      && buffer.count == capacity,
      "InputSpan identity mismatch"
    )
    let count = self.count
    discard self
    return count
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan {
  /// Consume the input span and return the number of initialized elements
  /// remaining at the end of the underlying memory region.
  ///
  /// This method should be invoked in the scope where the `InputSpan` was
  /// created, when it is time to commit the contents of the updated buffer
  /// back into the construct that was accessed.
  ///
  /// The context that created the input span is expected to remember what
  /// memory region the span is addressing. This consuming method expects to
  /// receive a copy of the same buffer pointer as a (loose) proof of ownership.
  ///
  /// - Parameter buffer: The buffer we expect the `InputSpan` to reference.
  ///      This must be the same region of memory passed to
  ///      the `InputSpan` initializer.
  /// - Returns: The number of initialized elements remaining at the end of
  ///      the same buffer, as tracked by the consumed `InputSpan` instance.
  @available(SwiftStdlib 6.5, *)
  @unsafe
  @export(implementation)
  public consuming func finalize(
    for buffer: Slice<UnsafeMutableBufferPointer<Element>>
  ) -> Int {
    unsafe finalize(for: UnsafeMutableBufferPointer(rebasing: buffer))
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan where Element: ~Copyable {
  /// Removes up to `maxCount` elements from the beginning of this span,
  /// and returns a new `InputSpan` that takes ownership of them.
  ///
  /// The returned span is a borrow of this span; when it goes out of scope,
  /// any elements it still holds are deinitialized.
  ///
  /// - Parameter maxCount: The maximum number of elements to consume.
  ///     `maxCount` must not be negative. If it exceeds `count`,
  ///     all elements are consumed.
  /// - Returns: An `InputSpan` containing the consumed elements.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(&self)
  public mutating func consumePrefix(
    maxCount: Int
  ) -> InputSpan<Element> {
    _precondition(
      maxCount >= 0, "Can't consume a negative number of elements"
    )
    let c = Swift.min(maxCount, count)
    let start: UnsafeMutablePointer<Element>? =
      unsafe _initializedStart()?.assumingMemoryBound(to: Element.self)
    let buffer = unsafe UnsafeMutableBufferPointer(start: start, count: c)
    _count &-= c
    let prefix = unsafe InputSpan(_uncheckedBuffer: buffer, initializedCount: c)
    return unsafe _overrideLifetime(prefix, mutating: &self)
  }

  /// Removes up to `maxCount` elements from the beginning of this span
  /// by passing them to a closure through an input span.
  ///
  /// The closure takes ownership of the elements: it can do anything with them,
  /// including nothing. Any elements still initialized when the input span
  /// goes out of scope are deinitialized, whether the closure returns normally
  /// or by throwing.
  ///
  /// The input span has a capacity equal to the number of elements it holds.
  ///
  /// - Parameters:
  ///   - maxCount: The maximum number of elements to consume.
  ///       `maxCount` must not be negative. If it exceeds `count`,
  ///       all elements are consumed.
  ///   - consumer: A closure that takes ownership of an `InputSpan`
  ///       containing the consumed elements.
  /// - Returns: The value returned by `consumer`.
  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public mutating func consumePrefix<E: Error, R: ~Copyable>(
    maxCount: Int,
    consumingWith consumer: (consuming InputSpan<Element>) throws(E) -> R
  ) throws(E) -> R {
    let prefix = consumePrefix(maxCount: maxCount)
    return try consumer(prefix)
  }
}

@available(SwiftStdlib 6.5, *)
extension InputSpan: Iterable where Element: ~Copyable {
  @available(SwiftStdlib 6.5, *)
  public typealias Failure = Never

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  public var underestimatedCount: Int {
    self.count
  }

  @available(SwiftStdlib 6.5, *)
  @export(implementation)
  @_lifetime(borrow self)
  public func makeBorrowingIterator() -> Span<Element>.BorrowingIterator {
    .init(self.span)
  }
}
