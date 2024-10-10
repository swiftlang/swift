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
@available(SwiftStdlib 6.1, *)
@frozen
@_rawLayout(likeArrayOf: Element, count: Count, movesAsLike)
public struct Vector<let Count: Int, Element: ~Copyable>: ~Copyable {}

@available(SwiftStdlib 6.1, *)
extension Vector: @unchecked Sendable where Element: Sendable & ~Copyable {}

//===----------------------------------------------------------------------===//
// Address & Buffer
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.1, *)
extension Vector where Element: ~Copyable {
  /// Returns a pointer to the first element in the vector.
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  var address: UnsafeMutablePointer<Element> {
    UnsafeMutablePointer<Element>(Builtin.addressOfRawLayout(self))
  }

  /// Returns a buffer pointer over the entire vector.
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  var buffer: UnsafeMutableBufferPointer<Element> {
    UnsafeMutableBufferPointer<Element>(start: address, count: Count)
  }
}

//===----------------------------------------------------------------------===//
// Initialization APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.1, *)
extension Vector where Element: ~Copyable {
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
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  public init<E: Error>(_ body: (Int) throws(E) -> Element) throws(E) {
    for i in 0 ..< Count {
      do {
        try buffer.initializeElement(at: i, to: body(i))
      } catch {
        // The closure threw an error. We need to deinitialize every element
        // we've initialized up to this point.
        for j in 0 ..< i {
          buffer.deinitializeElement(at: j)
        }

        // Throw the error we were given back out to the caller.
        throw error
      }
    }
  }
}

@available(SwiftStdlib 6.1, *)
extension Vector where Element: Copyable {
  /// Initializes every element in this vector to a copy of the given value.
  ///
  /// - Parameter value: The instance to initialize this vector with.
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  public init(repeating value: Element) {
    buffer.initialize(repeating: value)
  }
}

//===----------------------------------------------------------------------===//
// Copy
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.1, *)
extension Vector where Element: Copyable {
  /// Returns a fresh owned copy of the current vector.
  ///
  /// - Returns: A fresh owned copy of the current vector.
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  public borrowing func copy() -> Vector<Count, Element> {
    Vector<Count, Element> {
      self[$0]
    }
  }
}

//===----------------------------------------------------------------------===//
// RandomAccessCollection APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.1, *)
extension Vector where Element: ~Copyable {
  /// A type representing the collection's elements.
  @available(SwiftStdlib 6.1, *)
  public typealias Element = Element

  /// A type that represents a position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript
  /// argument.
  @available(SwiftStdlib 6.1, *)
  public typealias Index = Int

  /// A type that represents the indices that are valid for subscripting the
  /// collection, in ascending order.
  @available(SwiftStdlib 6.1, *)
  public typealias Indices = Range<Int>

  /// The number of elements in the collection.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var count: Int {
    Count
  }

  /// The position of the first element in a nonempty collection.
  ///
  /// If the collection is empty, `startIndex` is equal to `endIndex`.
  @available(SwiftStdlib 6.1, *)
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
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var endIndex: Int {
    Count
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
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  public var indices: Range<Int> {
    startIndex ..< endIndex
  }

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  /// - Returns: The index immediately after `i`.
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func index(after i: Int) -> Int {
    i + 1
  }

  /// Returns the position immediately before the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be greater than
  ///   `startIndex`.
  /// - Returns: The index value immediately before `i`.
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func index(before i: Int) -> Int {
    i - 1
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
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  public subscript(_ i: Int) -> Element {
    @_transparent
    unsafeAddress {
      _precondition(startIndex <= i && i < endIndex, "Index out of bounds")

      return UnsafePointer<Element>(address) + i
    }

    @_transparent
    unsafeMutableAddress {
      _precondition(startIndex <= i && i < endIndex, "Index out of bounds")

      return address + i
    }
  }
}

//===----------------------------------------------------------------------===//
// Unsafe APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.1, *)
extension Vector where Element: ~Copyable {
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
  ///     let numbers = Vector<5, Int> {
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
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  public borrowing func withUnsafeBufferPointer<Result, E: Error>(
    _ body: (UnsafeBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try body(UnsafeBufferPointer<Element>(buffer))
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
  ///     var numbers = Vector<5, Int> {
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
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  public mutating func withUnsafeMutableBufferPointer<Result, E: Error>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws(E) -> Result
  ) throws(E) -> Result {
    try body(buffer)
  }
}

//===----------------------------------------------------------------------===//
// Equatable
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.1, *)
extension Vector where Element: Equatable {
  /// Returns a Boolean value indicating whether two vectors contain the same
  /// elements in the same order.
  ///
  /// You can use the equal-to operator (`==`) to compare any two vectors
  /// that store the same, `Equatable`-conforming element type.
  ///
  /// - Parameters:
  ///   - lhs: A vector to compare.
  ///   - rhs: Another vector to compare.
  @available(SwiftStdlib 6.1, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func ==(
    lhs: borrowing Vector<Count, Element>,
    rhs: borrowing Vector<Count, Element>
  ) -> Bool {
    // No need for a count check because these are statically guaranteed to have
    // the same count...

    for i in 0 ..< Count {
      guard lhs[i] == rhs[i] else {
        return false
      }
    }

    return true
  }
}

//===----------------------------------------------------------------------===//
// CustomStringConvertible and CustomDebugStringConvertible APIs
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.1, *)
extension Vector where Element: CustomStringConvertible {
  /// A textual representation of the vector and its elements.
  @available(SwiftStdlib 6.1, *)
  public var description: String {
    var result = "["
    var isFirst = true

    for i in 0 ..< Count {
      if !isFirst {
        result += ", "
      } else {
        isFirst = false
      }

      result += self[i].description
    }

    result += "]"
    return result
  }
}

@available(SwiftStdlib 6.1, *)
extension Vector where Element: CustomDebugStringConvertible {
  /// A textual representation of the vector and its elements.
  @available(SwiftStdlib 6.1, *)
  public var debugDescription: String {
    var result = "["
    var isFirst = true

    for i in 0 ..< Count {
      if !isFirst {
        result += ", "
      } else {
        isFirst = false
      }

      result += self[i].debugDescription
    }

    result += "]"
    return result
  }
}
