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
/// An `InlineArray` is a specialized container that doesn't use a separate
/// memory allocation just to store its elements. When a value is copied, all of
/// its elements are copied eagerly, like those of a tuple. Use an `InlineArray`
/// when you have a fixed number of elements and need to avoid a separate heap
/// allocation.
///
/// Initializing a Value
/// --------------------
///
/// When initializing a new `InlineArray` value, you must initialize all of its
/// elements. You can use an array literal just as with `Array`, rely on type
/// inference for the `count` and `Element` type, and spell the type with the
/// shorthand `[count of Element]`.
///
///     let a: InlineArray<3, Int> = [1, 2, 3]
///     let b: InlineArray<_, Int> = [1, 2, 3]
///     let c: InlineArray<3, _>   = [1, 2, 3]
///     let d: InlineArray         = [1, 2, 3]
///     
///     let e: [3 of Int]          = [1, 2, 3]
///     let f: [_ of Int]          = [1, 2, 3]
///     let g: [3 of _]            = [1, 2, 3]
///     let h: [_ of _]            = [1, 2, 3]
///
/// You can also use one of the type's initializers to create a new value.
///
/// Accessing Elements
/// ------------------
///
/// Just as with `Array`, you can read and modify an element in an `InlineArray`
/// using a subscript. Unless you use the memory-unsafe `unchecked` subscript,
/// any index you provide is subject to bounds checking; an invalid index
/// triggers a runtime error in your program.
///
///     var values: [3 of Double] = [1, 1.5, 2]
///     print(values[0])  // Prints "1.0"
///     values[1] -= 0.25
///     print(values[1])  // Prints "1.25"
///     values[3] = 42.0  // Fatal error: Index out of bounds
///
/// You can use the `indices` property to iterate over all elements in order.
///
///     for index in values.indices {
///         print(values[index])
///     }
///
/// Working with Noncopyable Elements
/// ---------------------------------
///
/// An `InlineArray` can store elements of potentially noncopyable type. When
/// `Element` isn't copyable, the `InlineArray` itself also isn't copyable. You
/// must then explicitly move or consume the value if you want to transfer
/// ownership.
///
/// Memory Layout
/// -------------
///
/// An `InlineArray` stores its elements contiguously. If an `InlineArray` is a
/// stored property of a class, then it's allocated on the heap along with the
/// other stored properties of the class. Otherwise, in general, an
/// `InlineArray` is allocated on the stack.
///
/// A *non-empty* `InlineArray`'s size and stride are both found by multiplying
/// the `count` of elements by the `Element`'s stride. Its alignment is equal to
/// the `Element`'s alignment.
///
///     struct Record {
///         let x: UInt32
///         let y: Bool
///     }
///     MemoryLayout<Record>.size                 // 5
///     MemoryLayout<Record>.stride               // 8
///     MemoryLayout<Record>.alignment            // 4
///     MemoryLayout<[2 of Record]>.size          // 16
///     MemoryLayout<[2 of Record]>.stride        // 16
///     MemoryLayout<[2 of Record]>.alignment     // 4
///     MemoryLayout<(Record, Record)>.size       // 13
///     MemoryLayout<(Record, Record)>.stride     // 16
///     MemoryLayout<(Record, Record)>.alignment  // 4
///
/// An *empty* `InlineArray`'s size is zero. Its stride and alignment are both
/// one byte.
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
#if $AddressOfProperty2
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

  /// Returns a pointer to the first element in the array while performing stack
  /// checking.
  ///
  /// Use this when the value of the pointer could potentially be directly used
  /// by users (e.g. through the use of span or the unchecked subscript).
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _protectedAddress: UnsafePointer<Element> {
#if $AddressOfProperty2
    unsafe UnsafePointer<Element>(Builtin.addressOfBorrow(_storage))
#else
    unsafe UnsafePointer<Element>(Builtin.addressOfBorrow(self))
#endif
  }

  /// Returns a buffer pointer over the entire array while performing stack
  /// checking.
  ///
  /// Use this when the value of the pointer could potentially be directly used
  /// by users (e.g. through the use of span or the unchecked subscript).
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _protectedBuffer: UnsafeBufferPointer<Element> {
    unsafe UnsafeBufferPointer<Element>(start: _protectedAddress, count: count)
  }

  /// Returns a mutable pointer to the first element in the array.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _mutableAddress: UnsafeMutablePointer<Element> {
    mutating get {
#if $AddressOfProperty2
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

  /// Returns a mutable pointer to the first element in the array while
  /// performing stack checking.
  ///
  /// Use this when the value of the pointer could potentially be directly used
  /// by users (e.g. through the use of span or the unchecked subscript).
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _protectedMutableAddress: UnsafeMutablePointer<Element> {
    mutating get {
#if $AddressOfProperty2
      unsafe UnsafeMutablePointer<Element>(Builtin.addressof(&_storage))
#else
      unsafe UnsafeMutablePointer<Element>(Builtin.addressof(&self))
#endif
    }
  }

  /// Returns a mutable buffer pointer over the entire array while performing
  /// stack checking.
  ///
  /// Use this when the value of the pointer could potentially be directly used
  /// by users (e.g. through the use of span or the unchecked subscript).
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _protectedMutableBuffer: UnsafeMutableBufferPointer<Element> {
    mutating get {
      unsafe UnsafeMutableBufferPointer<Element>(
        start: _protectedMutableAddress,
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
  ///     InlineArray<4, Int> { $0 * 2 }  // [0, 2, 4, 6]
  ///
  /// The closure is allowed to throw an error at any point during
  /// initialization at which point the array will stop initialization,
  /// deinitialize every currently initialized element, and throw the given
  /// error back out to the caller.
  ///
  /// - Parameter body: A closure that returns an owned `Element` to emplace at
  ///   the passed in index.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init<E: Error>(_ body: (Index) throws(E) -> Element) throws(E) {
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
  }

  /// Initializes every element in this array, by calling the given closure
  /// with each preceding element.
  ///
  /// This will call the closure `count - 1` times, where `count` is the static
  /// count of the array, to initialize every element by passing the closure an
  /// immutable borrow reference to the preceding element.
  ///
  ///     InlineArray<4, Int>(first: 1) { $0 * 2 }  // [1, 2, 4, 8]
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
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init<E: Error>(
    first: consuming Element,
    next: (borrowing Element) throws(E) -> Element
  ) throws(E) {
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
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init<E: Error>(
    initializingWith initializer: (inout OutputSpan<Element>) throws(E) -> Void
  ) throws(E) {
    _storage = try Builtin.emplace { (rawPtr) throws(E) -> () in
      let buffer = unsafe Self._initializationBuffer(start: rawPtr)
      _internalInvariant(Self.count == buffer.count)
      var output = unsafe OutputSpan(buffer: buffer, initializedCount: 0)
      // no need to finalize in a `defer` block, since throwing will cause
      // changes to be deinitialized when the output span is deinited.
      try initializer(&output)
      let initialized = unsafe output.finalize(for: buffer)
      _precondition(count == initialized, "InlineArray initialization underflow")
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: Copyable {
  /// Initializes every element in this array to a copy of the given value.
  ///
  /// - Parameter value: The instance to initialize this array with.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init(repeating value: Element) {
    _storage = Builtin.emplace {
      let buffer = unsafe Self._initializationBuffer(start: $0)

      unsafe buffer.initialize(repeating: value)
    }
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
      unsafe _protectedAddress + i
    }

    @_transparent
    unsafeMutableAddress {
      unsafe _protectedMutableAddress + i
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
// MARK: - Span APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public var span: Span<Element> {
    @lifetime(borrow self)
    @_transparent
    borrowing get {
      guard count > 0 else {
        let span = Span<Element>()
        return unsafe _overrideLifetime(span, borrowing: self)
      }
      let span = unsafe Span(_unsafeStart: _protectedAddress, count: count)
      return unsafe _overrideLifetime(span, borrowing: self)
    }
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public var mutableSpan: MutableSpan<Element> {
    @lifetime(&self)
    @_transparent
    mutating get {
      guard count > 0 else {
        let span = MutableSpan<Element>()
        return unsafe _overrideLifetime(span, mutating: &self)
      }
      let span = unsafe MutableSpan(
        _unsafeStart: _protectedMutableAddress,
        count: count
      )
      return unsafe _overrideLifetime(span, mutating: &self)
    }
  }
}
