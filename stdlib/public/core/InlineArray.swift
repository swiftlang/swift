//===----------------------------------------------------------------------===//
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

/// A fixed-size array.
///
/// The `InlineArray` type is a specialized array that stores its elements
/// contiguously inline, rather than allocating an out-of-line region of memory
/// with copy-on-write optimization.
///
/// Memory Layout
/// -------------
///
/// An *empty* array's size is zero. Its stride and alignment are one byte.
///
/// A *nonempty* array's size and stride are equal to the element's stride
/// multiplied by the number of elements. Its alignment is equal to the
/// element's alignment.
///
///     MemoryLayout<InlineArray<3, UInt16>>.size       //-> 6
///     MemoryLayout<InlineArray<3, UInt16>>.stride     //-> 6
///     MemoryLayout<InlineArray<3, UInt16>>.alignment  //-> 2
///
/// Literal Initialization
/// ----------------------
///
/// Array literal syntax can be used to initialize an `InlineArray` instance.
/// A stack-allocated array will do in-place initialization of each element.
/// The `count` and/or `Element` can be inferred from the array literal.
///
///     let a: InlineArray<4, Int> = [1, 2, 4, 8]
///     let b: InlineArray<_, Int> = [1, 2, 4, 8]
///     let c: InlineArray<4, _>   = [1, 2, 4, 8]
///     let d: InlineArray         = [1, 2, 4, 8]
@available(SwiftStdlib 6.2, *)
@frozen
@safe
@_addressableForDependencies
public struct InlineArray<let count: Int, Element: ~Copyable>: ~Copyable {
  @usableFromInline
  internal var _storage: Builtin.FixedArray<count, Element>
}

@available(SwiftStdlib 6.2, *)
extension InlineArray: Copyable where Element: Copyable {}

@available(SwiftStdlib 6.2, *)
extension InlineArray: BitwiseCopyable where Element: BitwiseCopyable {}

@available(SwiftStdlib 6.2, *)
extension InlineArray: @unchecked Sendable where Element: Sendable & ~Copyable {}

//===----------------------------------------------------------------------===//
// MARK: - Address & Buffer
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// Returns a pointer to the first element in the array.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _address: UnsafePointer<Element> {
#if $AddressOfProperty
    unsafe UnsafePointer<Element>(Builtin.unprotectedAddressOfBorrow(_storage))
#else
    unsafe UnsafePointer<Element>(Builtin.unprotectedAddressOfBorrow(self))
#endif
  }

  /// Returns a buffer pointer over the entire array.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _buffer: UnsafeBufferPointer<Element> {
    unsafe UnsafeBufferPointer<Element>(start: _address, count: count)
  }

  /// Returns a mutable pointer to the first element in the array.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _mutableAddress: UnsafeMutablePointer<Element> {
    mutating get {
#if $AddressOfProperty
      unsafe UnsafeMutablePointer<Element>(Builtin.unprotectedAddressOf(&_storage))
#else
      unsafe UnsafeMutablePointer<Element>(Builtin.unprotectedAddressOf(&self))
#endif
    }
  }

  /// Returns a mutable buffer pointer over the entire array.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _mutableBuffer: UnsafeMutableBufferPointer<Element> {
    mutating get {
      unsafe UnsafeMutableBufferPointer<Element>(
        start: _mutableAddress,
        count: count
      )
    }
  }

  /// Converts the given raw pointer, which points at an uninitialized array
  /// instance, to a mutable buffer suitable for initialization.
  @available(SwiftStdlib 6.2, *)
  @unsafe
  @_alwaysEmitIntoClient
  @_transparent
  internal static func _initializationBuffer(
    start: Builtin.RawPointer
  ) -> UnsafeMutableBufferPointer<Element> {
    unsafe UnsafeMutableBufferPointer<Element>(
      start: UnsafeMutablePointer<Element>(start),
      count: count
    )
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Initialization APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// Initializes every element in this array, by calling the given closure
  /// with each index.
  ///
  /// This will call the closure `count` times, where `count` is the static
  /// count of the array, to initialize every element by passing the closure
  /// the index of the current element being initialized.
  ///
  ///     InlineArray<4, Int> { 1 << $0 }  //-> [1, 2, 4, 8]
  ///
  /// The closure is allowed to throw an error at any point during
  /// initialization at which point the array will stop initialization,
  /// deinitialize every currently initialized element, and throw the given
  /// error back out to the caller.
  ///
  /// - Parameter body: A closure that returns an owned `Element` to emplace at
  ///   the passed in index.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the array.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init<E: Error>(_ body: (Index) throws(E) -> Element) throws(E) {
#if $BuiltinEmplaceTypedThrows
    _storage = try Builtin.emplace { (rawPtr) throws(E) -> () in
      let buffer = unsafe Self._initializationBuffer(start: rawPtr)

      for i in 0 ..< count {
        do throws(E) {
          try unsafe buffer.initializeElement(at: i, to: body(i))
        } catch {
          // The closure threw an error. We need to deinitialize every element
          // we've initialized up to this point.
          for j in 0 ..< i {
            unsafe buffer.deinitializeElement(at: j)
          }

          // Throw the error we were given back out to the caller.
          throw error
        }
      }
    }
#else
    fatalError()
#endif
  }

  /// Initializes every element in this array, by calling the given closure
  /// with each preceding element.
  ///
  /// This will call the closure `count - 1` times, where `count` is the static
  /// count of the array, to initialize every element by passing the closure an
  /// immutable borrow reference to the preceding element.
  ///
  ///     InlineArray<4, Int>(first: 1) { $0 << 1 }  //-> [1, 2, 4, 8]
  ///
  /// The closure is allowed to throw an error at any point during
  /// initialization at which point the array will stop initialization,
  /// deinitialize every currently initialized element, and throw the given
  /// error back out to the caller.
  ///
  /// - Parameters:
  ///   - first: The first value to emplace into the array.
  ///   - next: A closure that takes an immutable borrow reference to the
  ///     preceding element, and returns an owned `Element` instance to emplace
  ///     into the array.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the array.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init<E: Error>(
    first: consuming Element,
    next: (borrowing Element) throws(E) -> Element
  ) throws(E) {
#if $BuiltinEmplaceTypedThrows
    // FIXME: We should be able to mark 'Builtin.emplace' as '@once' or something
    //        to give the compiler enough information to know we will only run
    //        it once so it can consume the capture. For now, we use an optional
    //        and take the underlying value within the closure.
    var o: Element? = first

    _storage = try Builtin.emplace { (rawPtr) throws(E) -> () in
      let buffer = unsafe Self._initializationBuffer(start: rawPtr)

      guard Self.count > 0 else {
        return
      }

      unsafe buffer.initializeElement(
        at: 0,
        to: o.take()._consumingUncheckedUnwrapped()
      )

      for i in 1 ..< count {
        do throws(E) {
          try unsafe buffer.initializeElement(at: i, to: next(buffer[i &- 1]))
        } catch {
          // The closure threw an error. We need to deinitialize every element
          // we've initialized up to this point.
          for j in 0 ..< i {
            unsafe buffer.deinitializeElement(at: j)
          }

          // Throw the error we were given back out to the caller.
          throw error
        }
      }
    }
#else
    fatalError()
#endif
  }
}

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: Copyable {
  /// Initializes every element in this array to a copy of the given value.
  ///
  /// - Parameter value: The instance to initialize this array with.
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the array.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init(repeating value: Element) {
#if $ValueGenericsNameLookup
    _storage = Builtin.emplace {
      let buffer = unsafe Self._initializationBuffer(start: $0)

      unsafe buffer.initialize(repeating: value)
    }
#else
    fatalError()
#endif
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Collection APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// The type of the array's elements.
  @available(SwiftStdlib 6.2, *)
  public typealias Element = Element

  /// A type that represents a position in the array.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript
  /// argument.
  @available(SwiftStdlib 6.2, *)
  public typealias Index = Int

  /// The number of elements in the array.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_semantics("fixed_storage.get_count")
  @inline(__always)
  public var count: Int {
    count
  }

  /// A Boolean value indicating whether the array is empty.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var isEmpty: Bool {
    count == 0
  }

  /// The position of the first element in a nonempty array.
  ///
  /// If the array is empty, `startIndex` is equal to `endIndex`.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var startIndex: Index {
    0
  }

  /// The array's "past the end" position---that is, the position one greater
  /// than the last valid subscript argument.
  ///
  /// If the array is empty, `endIndex` is equal to `startIndex`.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var endIndex: Index {
    count
  }

  /// The indices that are valid for subscripting the array, in ascending order.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var indices: Range<Index> {
    unsafe Range(_uncheckedBounds: (0, count))
  }

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the array. `i` must be less than
  ///   `endIndex`.
  /// - Returns: The index immediately after `i`.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func index(after i: Index) -> Index {
    i &+ 1
  }

  /// Returns the position immediately before the given index.
  ///
  /// - Parameter i: A valid index of the array. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func index(before i: Index) -> Index {
    i &- 1
  }

  @_alwaysEmitIntoClient
  @_semantics("fixed_storage.check_index")
  @inline(__always)
  internal func _checkIndex(_ i: Index) {
    _precondition(indices.contains(i), "Index out of bounds")
  }

  /// Accesses the element at the specified position.
  ///
  /// - Parameter i: The position of the element to access. `i` must be a valid
  ///   index of the array that is not equal to the `endIndex` property.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_addressableSelf
  @_alwaysEmitIntoClient
  public subscript(_ i: Index) -> Element {
    @_transparent
    unsafeAddress {
      _checkIndex(i)
      return unsafe _address + i
    }

    @_transparent
    unsafeMutableAddress {
      _checkIndex(i)
      return unsafe _mutableAddress + i
    }
  }

  /// Accesses the element at the specified position.
  ///
  /// - Warning: This subscript trades safety for performance. Using an invalid
  ///   index results in undefined behavior.
  ///
  /// - Parameter i: The position of the element to access. `i` must be a valid
  ///   index of the array that is not equal to the `endIndex` property.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_addressableSelf
  @_alwaysEmitIntoClient
  @unsafe
  public subscript(unchecked i: Index) -> Element {
    @_transparent
    unsafeAddress {
      unsafe _address + i
    }

    @_transparent
    unsafeMutableAddress {
      unsafe _mutableAddress + i
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: - MutableCollection APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// Exchanges the values at the specified indices of the array.
  ///
  /// Both parameters must be valid indices of the array and not equal to
  /// `endIndex`. Passing the same index as both `i` and `j` has no effect.
  ///
  /// - Parameters:
  ///   - i: The index of the first value to swap.
  ///   - j: The index of the second value to swap.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func swapAt(
    _ i: Index,
    _ j: Index
  ) {
    guard i != j else {
      return
    }

    _checkIndex(i)
    _checkIndex(j)

    let ithElement = unsafe _mutableBuffer.moveElement(from: i)
    let jthElement = unsafe _mutableBuffer.moveElement(from: j)
    unsafe _mutableBuffer.initializeElement(at: i, to: jthElement)
    unsafe _mutableBuffer.initializeElement(at: j, to: ithElement)
  }
}

//===----------------------------------------------------------------------===//
// MARK: Span
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {

  @available(SwiftStdlib 6.2, *)
  public var span: Span<Element> {
    @lifetime(borrow self)
    @_alwaysEmitIntoClient
    borrowing get {
      let pointer = unsafe _address
      let span = unsafe Span(_unsafeStart: pointer, count: count)
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  @available(SwiftStdlib 6.2, *)
  public var mutableSpan: MutableSpan<Element> {
    @lifetime(&self)
    @_alwaysEmitIntoClient
    mutating get {
      let pointer = unsafe _mutableAddress
      let span = unsafe MutableSpan(_unsafeStart: pointer, count: count)
      return unsafe _overrideLifetime(span, mutating: &self)
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Unsafe APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  // FIXME: @available(*, deprecated, renamed: "span.withUnsafeBufferPointer(_:)")
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func _withUnsafeBufferPointer<Result: ~Copyable, E: Error>(
    _ body: (UnsafeBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe body(_buffer)
  }

  // FIXME: @available(*, deprecated, renamed: "mutableSpan.withUnsafeMutableBufferPointer(_:)")
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public mutating func _withUnsafeMutableBufferPointer<Result: ~Copyable, E: Error>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe body(_mutableBuffer)
  }
}
