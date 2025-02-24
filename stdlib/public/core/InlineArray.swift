//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A fixed-size array.
@available(SwiftStdlib 6.2, *)
@frozen
@safe
public struct InlineArray<let count: Int, Element: ~Copyable>: ~Copyable {
  @usableFromInline
  internal let _storage: Builtin.FixedArray<count, Element>
}

@available(SwiftStdlib 6.2, *)
extension InlineArray: Copyable where Element: Copyable {}

@available(SwiftStdlib 6.2, *)
extension InlineArray: BitwiseCopyable where Element: BitwiseCopyable {}

@available(SwiftStdlib 6.2, *)
extension InlineArray: @unchecked Sendable where Element: Sendable & ~Copyable {}

//===----------------------------------------------------------------------===//
// Address & Buffer
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// Returns a read-only pointer to the first element in the vector.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _address: UnsafePointer<Element> {
    unsafe UnsafePointer<Element>(Builtin.unprotectedAddressOfBorrow(self))
  }

  /// Returns a buffer pointer over the entire vector.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _buffer: UnsafeBufferPointer<Element> {
    unsafe UnsafeBufferPointer<Element>(start: _address, count: count)
  }

  /// Returns a mutable pointer to the first element in the vector.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _mutableAddress: UnsafeMutablePointer<Element> {
    mutating get {
      unsafe UnsafeMutablePointer<Element>(Builtin.unprotectedAddressOf(&self))
    }
  }

  /// Returns a mutable buffer pointer over the entire vector.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal var _mutableBuffer: UnsafeMutableBufferPointer<Element> {
    mutating get {
      unsafe UnsafeMutableBufferPointer<Element>(start: _mutableAddress, count: count)
    }
  }

  /// Returns the given raw pointer, which points at an uninitialized vector
  /// instance, to a mutable buffer suitable for initialization.
  @available(SwiftStdlib 6.2, *)
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
// Initialization APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// Initializes every element in this vector running the given closure value
  /// that returns the element to emplace at the given index.
  ///
  /// This will call the closure `Count` times, where `Count` is the static
  /// count of the vector, to initialize every element by passing the closure
  /// the index of the current element being initialized. The closure is allowed
  /// to throw an error at any point during initialization at which point the
  /// vector will stop initialization, deinitialize every currently initialized
  /// element, and throw the given error back out to the caller.
  ///
  /// - Parameter body: A closure that returns an owned `Element` to emplace at
  ///                   the passed in index.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init<E: Error>(_ body: (Int) throws(E) -> Element) throws(E) {
#if $BuiltinEmplaceTypedThrows
    self = try Builtin.emplace { (rawPtr) throws(E) -> () in
      let buffer = InlineArray<count, Element>._initializationBuffer(
        start: rawPtr
      )

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

  /// Initializes every element in this vector by running the closure with the
  /// passed in first element.
  ///
  /// This will call the closure 'count' times, where 'count' is the static
  /// count of the vector, to initialize every element by passing the closure
  /// an immutable borrow reference to the first element given to the
  /// initializer. The closure is allowed to throw an error at any point during
  /// initialization at which point the vector will stop initialization,
  /// deinitialize every currently initialized element, and throw the given
  /// error back out to the caller.
  ///
  /// - Parameter first: The first value to insert into the vector which will be
  ///                    passed to the closure as a borrow.
  /// - Parameter next: A closure that passes in an immutable borrow reference
  ///                   of the given first element of the vector which returns
  ///                   an owned `Element` instance to insert into the vector.
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

    self = try Builtin.emplace { (rawPtr) throws(E) -> () in
      let buffer = InlineArray<count, Element>._initializationBuffer(
        start: rawPtr
      )

      unsafe buffer.initializeElement(at: 0, to: o.take()._consumingUncheckedUnwrapped())

      for i in 1 ..< count {
        do throws(E) {
          try unsafe buffer.initializeElement(at: i, to: next(buffer[i &- 1]))
        } catch {
          // The closure threw an error. We need to deinitialize every element
          // we've initialized up to this point.
          for j in 0 ..< i {
            unsafe buffer.deinitializeElement(at: j)
          }

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
  /// Initializes every element in this vector to a copy of the given value.
  ///
  /// - Parameter value: The instance to initialize this vector with.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public init(repeating value: Element) {
    self = Builtin.emplace {
      let buffer = InlineArray<count, Element>._initializationBuffer(start: $0)

      unsafe buffer.initialize(repeating: value)
    }
  }
}

//===----------------------------------------------------------------------===//
// Collection APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// The type of the container's elements.
  @available(SwiftStdlib 6.2, *)
  public typealias Element = Element

  /// A type that represents a position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript
  /// argument.
  @available(SwiftStdlib 6.2, *)
  public typealias Index = Int

  /// The number of elements in the collection.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static var count: Int {
    count
  }

  /// The number of elements in the collection.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var count: Int {
    count
  }

  /// The position of the first element in a nonempty collection.
  ///
  /// If the collection is empty, `startIndex` is equal to `endIndex`.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var startIndex: Int {
    0
  }

  /// The collection's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// When you need a range that includes the last element of a collection, use
  /// the half-open range operator (`..<`) with `endIndex`. The `..<` operator
  /// creates a range that doesn't include the upper bound, so it's always
  /// safe to use with `endIndex`. For example:
  ///
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     if let index = numbers.firstIndex(of: 30) {
  ///         print(numbers[index ..< numbers.endIndex])
  ///     }
  ///     // Prints "[30, 40, 50]"
  ///
  /// If the collection is empty, `endIndex` is equal to `startIndex`.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var endIndex: Int {
    count
  }

  /// The indices that are valid for subscripting the collection, in ascending
  /// order.
  ///
  /// A collection's `indices` property can hold a strong reference to the
  /// collection itself, causing the collection to be nonuniquely referenced.
  /// If you mutate the collection while iterating over its indices, a strong
  /// reference can result in an unexpected copy of the collection. To avoid
  /// the unexpected copy, use the `index(after:)` method starting with
  /// `startIndex` to produce indices instead.
  ///
  ///     var c = MyFancyCollection([10, 20, 30, 40, 50])
  ///     var i = c.startIndex
  ///     while i != c.endIndex {
  ///         c[i] /= 5
  ///         i = c.index(after: i)
  ///     }
  ///     // c == MyFancyCollection([2, 4, 6, 8, 10])
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var indices: Range<Int> {
    Range(_uncheckedBounds: (0, count))
  }

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  /// - Returns: The index immediately after `i`.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func index(after i: Int) -> Int {
    i &+ 1
  }

  /// Returns the position immediately before the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func index(before i: Int) -> Int {
    i &- 1
  }

  /// Accesses the element at the specified position.
  ///
  /// The following example accesses an element of an array through its
  /// subscript to print its value:
  ///
  ///     var streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     print(streets[1])
  ///     // Prints "Bryant"
  ///
  /// You can subscript a collection with any valid index other than the
  /// collection's end index. The end index refers to the position one past
  /// the last element of a collection, so it doesn't correspond with an
  /// element.
  ///
  /// - Parameter position: The position of the element to access. `position`
  ///   must be a valid index of the collection that is not equal to the
  ///   `endIndex` property.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_addressableSelf
  @_alwaysEmitIntoClient
  public subscript(_ i: Int) -> Element {
    @_transparent
    unsafeAddress {
      _precondition(indices.contains(i), "Index out of bounds")

      return unsafe _address + i
    }

    @_transparent
    unsafeMutableAddress {
      _precondition(indices.contains(i), "Index out of bounds")

      return unsafe _mutableAddress + i
    }
  }
}

//===----------------------------------------------------------------------===//
// Swap
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// Exchanges the values at the specified indices of the vector.
  ///
  /// Both parameters must be valid indices of the vector and not
  /// equal to `endIndex`. Passing the same index as both `i` and `j` has no
  /// effect.
  ///
  /// - Parameters:
  ///   - i: The index of the first value to swap.
  ///   - j: The index of the second value to swap.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public mutating func swapAt(
    _ i: Int,
    _ j: Int
  ) {
    guard i != j else {
      return
    }

    _precondition(indices.contains(i), "Index out of bounds")
    _precondition(indices.contains(j), "Index out of bounds")

    let ithElement = unsafe _mutableBuffer.moveElement(from: i)
    let jthElement = unsafe _mutableBuffer.moveElement(from: j)
    unsafe _mutableBuffer.initializeElement(at: i, to: jthElement)
    unsafe _mutableBuffer.initializeElement(at: j, to: ithElement)
  }
}

//===----------------------------------------------------------------------===//
// Unsafe APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.2, *)
extension InlineArray where Element: ~Copyable {
  /// Calls a closure with a pointer to the vector's contiguous storage.
  ///
  /// Often, the optimizer can eliminate bounds checks within a vector
  /// algorithm, but when that fails, invoking the same algorithm on the
  /// buffer pointer passed into your closure lets you trade safety for speed.
  ///
  /// The following example shows how you can iterate over the contents of the
  /// buffer pointer:
  ///
  ///     // "[1, 2, 3, 4, 5]"
  ///     let numbers = InlineArray<5, Int> {
  ///       $0 + 1
  ///     }
  ///
  ///     let sum = numbers.withUnsafeBufferPointer { buffer -> Int in
  ///         var result = 0
  ///         for i in stride(from: buffer.startIndex, to: buffer.endIndex, by: 2) {
  ///             result += buffer[i]
  ///         }
  ///         return result
  ///     }
  ///     // 'sum' == 9
  ///
  /// The pointer passed as an argument to `body` is valid only during the
  /// execution of `withUnsafeBufferPointer(_:)`. Do not store or return the
  /// pointer for later use.
  ///
  /// - Parameter body: A closure with an `UnsafeBufferPointer` parameter that
  ///   points to the contiguous storage for the vector. If `body` has a return
  ///   value, that value is also used as the return value for the
  ///   `withUnsafeBufferPointer(_:)` method. The pointer argument is valid only
  ///   for the duration of the method's execution.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func _withUnsafeBufferPointer<Result: ~Copyable, E: Error>(
    _ body: (UnsafeBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe body(_buffer)
  }

  /// Calls the given closure with a pointer to the vector's mutable contiguous
  /// storage.
  ///
  /// Often, the optimizer can eliminate bounds checks within a vector
  /// algorithm, but when that fails, invoking the same algorithm on the
  /// buffer pointer passed into your closure lets you trade safety for speed.
  ///
  /// The following example shows how modifying the contents of the
  /// `UnsafeMutableBufferPointer` argument to `body` alters the contents of
  /// the vector:
  ///
  ///     // "[1, 2, 3, 4, 5]"
  ///     var numbers = InlineArray<5, Int> {
  ///       $0 + 1
  ///     }
  ///
  ///     numbers.withUnsafeMutableBufferPointer { buffer in
  ///         for i in stride(from: buffer.startIndex, to: buffer.endIndex - 1, by: 2) {
  ///             buffer.swapAt(i, i + 1)
  ///         }
  ///     }
  ///
  ///     print(numbers.description)
  ///     // Prints "[2, 1, 4, 3, 5]"
  ///
  /// The pointer passed as an argument to `body` is valid only during the
  /// execution of `withUnsafeMutableBufferPointer(_:)`. Do not store or
  /// return the pointer for later use.
  ///
  /// - Warning: Do not rely on anything about the vector that is the target of
  ///   this method during execution of the `body` closure; it might not
  ///   appear to have its correct value. Instead, use only the
  ///   `UnsafeMutableBufferPointer` argument to `body`.
  ///
  /// - Parameter body: A closure with an `UnsafeMutableBufferPointer`
  ///   parameter that points to the contiguous storage for the vector. If
  ///   `body` has a return value, that value is also used as the return value
  ///   for the `withUnsafeMutableBufferPointer(_:)` method. The pointer
  ///   argument is valid only for the duration of the method's execution.
  /// - Returns: The return value, if any, of the `body` closure parameter.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  @_transparent
  public mutating func _withUnsafeMutableBufferPointer<Result: ~Copyable, E: Error>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try unsafe body(_mutableBuffer)
  }
}
