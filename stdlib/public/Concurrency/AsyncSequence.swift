//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A type that provides asynchronous, sequential, iterated access to its
/// elements.
///
/// An `AsyncSequence` resembles the `Sequence` type --- offering a list of
/// values you can step through one at a time --- and adds asynchronicity. An
/// `AsyncSequence` may have all, some, or none of its values available when
/// you first use it. Instead, you use `await` to receive values as they become
/// available.
///
/// As with `Sequence`, you typically iterate through an `AsyncSequence` with a
/// `for await`-`in` loop. However, because the caller must potentially wait for values,
/// you use the `await` keyword. The following example shows how to iterate
/// over `Counter`, a custom `AsyncSequence` that produces `Int` values from
/// `1` up to a `howHigh` value:
///
///     for await number in Counter(howHigh: 10) {
///         print(number, terminator: " ")
///     }
///     // Prints "1 2 3 4 5 6 7 8 9 10 "
///
/// An `AsyncSequence` doesn't generate or contain the values; it just defines
/// how you access them. Along with defining the type of values as an associated
/// type called `Element`, the `AsyncSequence` defines a `makeAsyncIterator()`
/// method. This returns an instance of type `AsyncIterator`. Like the standard
/// `IteratorProtocol`, the `AsyncIteratorProtocol` defines a single `next()`
/// method to produce elements. The difference is that the `AsyncIterator`
/// defines its `next()` method as `async`, which requires a caller to wait for
/// the next value with the `await` keyword.
///
/// `AsyncSequence` also defines methods for processing the elements you
/// receive, modeled on the operations provided by the basic `Sequence` in the
/// standard library. There are two categories of methods: those that return a
/// single value, and those that return another `AsyncSequence`.
///
/// Single-value methods eliminate the need for a `for await`-`in` loop, and instead
/// let you make a single `await` call. For example, the `contains(_:)` method
/// returns a Boolean value that indicates if a given value exists in the
/// `AsyncSequence`. Given the `Counter` sequence from the previous example,
/// you can test for the existence of a sequence member with a one-line call:
///
///     let found = await Counter(howHigh: 10).contains(5) // true
///
/// Methods that return another `AsyncSequence` return a type specific to the
/// method's semantics. For example, the `.map(_:)` method returns a
/// `AsyncMapSequence` (or a `AsyncThrowingMapSequence`, if the closure you
/// provide to the `map(_:)` method can throw an error). These returned
/// sequences don't eagerly await the next member of the sequence, which allows
/// the caller to decide when to start work. Typically, you'll iterate over
/// these sequences with `for await`-`in`, like the base `AsyncSequence` you started
/// with. In the following example, the `map(_:)` method transforms each `Int`
/// received from a `Counter` sequence into a `String`:
///
///     let stream = Counter(howHigh: 10)
///         .map { $0 % 2 == 0 ? "Even" : "Odd" }
///     for await s in stream {
///         print(s, terminator: " ")
///     }
///     // Prints "Odd Even Odd Even Odd Even Odd Even Odd Even "
///
@available(SwiftStdlib 5.1, *)
public protocol AsyncSequence<Element, Failure> {
  /// The type of asynchronous iterator that produces elements of this
  /// asynchronous sequence.
  associatedtype AsyncIterator: AsyncIteratorProtocol where AsyncIterator.Element == Element
  /// The type of element produced by this asynchronous sequence.
  associatedtype Element

  /// The type of errors produced when iteration over the sequence fails.
  @available(SwiftStdlib 6.0, *)
  associatedtype Failure: Error = any Error
      where AsyncIterator.Failure == Failure

  /// Creates the asynchronous iterator that produces elements of this
  /// asynchronous sequence.
  ///
  /// - Returns: An instance of the `AsyncIterator` type used to produce
  /// elements of the asynchronous sequence.
  __consuming func makeAsyncIterator() -> AsyncIterator
}

@available(SwiftStdlib 5.1, *)
extension AsyncSequence {
  /// Returns the result of combining the elements of the asynchronous sequence
  /// using the given closure.
  ///
  /// Use the `reduce(_:_:)` method to produce a single value from the elements of
  /// an entire sequence. For example, you can use this method on an sequence of
  /// numbers to find their sum or product.
  ///
  /// The `nextPartialResult` closure executes sequentially with an accumulating
  /// value initialized to `initialResult` and each element of the sequence.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `4`. The `reduce(_:_:)` method sums the values
  /// received from the asynchronous sequence.
  ///
  ///     let sum = await Counter(howHigh: 4)
  ///         .reduce(0) {
  ///             $0 + $1
  ///         }
  ///     print(sum)
  ///     // Prints "10"
  ///
  ///
  /// - Parameters:
  ///   - initialResult: The value to use as the initial accumulating value.
  ///     The `nextPartialResult` closure receives `initialResult` the first
  ///     time the closure runs.
  ///   - nextPartialResult: A closure that combines an accumulating value and
  ///     an element of the asynchronous sequence into a new accumulating value,
  ///     for use in the next call of the `nextPartialResult` closure or
  ///     returned to the caller.
  /// - Returns: The final accumulated value. If the sequence has no elements,
  ///   the result is `initialResult`.
  @inlinable
  public func reduce<Result>(
    _ initialResult: Result,
    _ nextPartialResult:
      (_ partialResult: Result, Element) async throws -> Result
  ) async rethrows -> Result {
    var accumulator = initialResult
    var iterator = makeAsyncIterator()
    while let element = try await iterator.next() {
      accumulator = try await nextPartialResult(accumulator, element)
    }
    return accumulator
  }

  /// Returns the result of combining the elements of the asynchronous sequence
  /// using the given closure, given a mutable initial value.
  ///
  /// Use the `reduce(into:_:)` method to produce a single value from the
  /// elements of an entire sequence. For example, you can use this method on a
  /// sequence of numbers to find their sum or product.
  ///
  /// The `updateAccumulatingResult` closure executes sequentially with an
  /// accumulating value initialized to `initialResult` and each element of the
  /// sequence.
  ///
  /// Prefer this method over `reduce(_:_:)` for efficiency when the result is
  /// a copy-on-write type, for example an `Array` or `Dictionary`.
  ///
  /// - Parameters:
  ///   - initialResult: The value to use as the initial accumulating value.
  ///     The `nextPartialResult` closure receives `initialResult` the first
  ///     time the closure executes.
  ///   - updateAccumulatingResult: A closure that combines an accumulating
  ///     value and an element of the asynchronous sequence into a new
  ///     accumulating value, for use in the next call of the
  ///     `nextPartialResult` closure or returned to the caller.
  /// - Returns: The final accumulated value. If the sequence has no elements,
  ///   the result is `initialResult`.
  @inlinable
  public func reduce<Result>(
    into initialResult: __owned Result,
    _ updateAccumulatingResult:
      (_ partialResult: inout Result, Element) async throws -> Void
  ) async rethrows -> Result {
    var accumulator = initialResult
    var iterator = makeAsyncIterator()
    while let element = try await iterator.next() {
      try await updateAccumulatingResult(&accumulator, element)
    }
    return accumulator
  }
}

@available(SwiftStdlib 5.1, *)
@inlinable
@inline(__always)
func _contains<Source: AsyncSequence>(
  _ self: Source,
  where predicate: (Source.Element) async throws -> Bool
) async rethrows -> Bool {
  for try await element in self {
    if try await predicate(element) {
      return true
    }
  }
  return false
}

@available(SwiftStdlib 5.1, *)
extension AsyncSequence {
  /// Returns a Boolean value that indicates whether the asynchronous sequence
  /// contains an element that satisfies the given predicate.
  ///
  /// You can use the predicate to check for an element of a type that doesn’t
  /// conform to the `Equatable` protocol, or to find an element that satisfies
  /// a general condition.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `10`. The `contains(where:)` method checks to see
  /// whether the sequence produces a value divisible by `3`:
  ///
  ///     let containsDivisibleByThree = await Counter(howHigh: 10)
  ///         .contains { $0 % 3 == 0 }
  ///     print(containsDivisibleByThree)
  ///     // Prints "true"
  ///
  /// The predicate executes each time the asynchronous sequence produces an
  /// element, until either the predicate finds a match or the sequence ends.
  ///
  /// - Parameter predicate: A closure that takes an element of the asynchronous
  ///   sequence as its argument and returns a Boolean value that indicates
  ///   whether the passed element represents a match.
  /// - Returns: `true` if the sequence contains an element that satisfies
  ///   predicate; otherwise, `false`.
  @inlinable
  public func contains(
    where predicate: (Element) async throws -> Bool
  ) async rethrows -> Bool {
    return try await _contains(self, where: predicate)
  }
  
  /// Returns a Boolean value that indicates whether all elements produced by the
  /// asynchronous sequence satisfy the given predicate.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `10`. The `allSatisfy(_:)` method checks to see whether
  /// all elements produced by the sequence are less than `10`.
  ///
  ///     let allLessThanTen = await Counter(howHigh: 10)
  ///         .allSatisfy { $0 < 10 }
  ///     print(allLessThanTen)
  ///     // Prints "false"
  ///
  /// The predicate executes each time the asynchronous sequence produces an
  /// element, until either the predicate returns `false` or the sequence ends.
  ///
  /// If the asynchronous sequence is empty, this method returns `true`.
  ///
  /// - Parameter predicate: A closure that takes an element of the asynchronous
  ///   sequence as its argument and returns a Boolean value that indicates
  ///   whether the passed element satisfies a condition.
  /// - Returns: `true` if the sequence contains only elements that satisfy
  ///   `predicate`; otherwise, `false`.
  @inlinable
  public func allSatisfy(
    _ predicate: (Element) async throws -> Bool
  ) async rethrows -> Bool {
    return try await !contains { try await !predicate($0) }
  }
}

@available(SwiftStdlib 5.1, *)
extension AsyncSequence where Element: Equatable {
  /// Returns a Boolean value that indicates whether the asynchronous sequence
  /// contains the given element.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `10`. The `contains(_:)` method checks to see whether
  /// the sequence produces the value `5`:
  ///
  ///     let containsFive = await Counter(howHigh: 10)
  ///         .contains(5)
  ///     print(containsFive)
  ///     // Prints "true"
  ///
  /// - Parameter search: The element to find in the asynchronous sequence.
  /// - Returns: `true` if the method found the element in the asynchronous
  ///   sequence; otherwise, `false`.
  @inlinable
  public func contains(_ search: Element) async rethrows -> Bool {
    for try await element in self {
      if element == search {
        return true
      }
    }
    return false
  }
}

@available(SwiftStdlib 5.1, *)
@inlinable
@inline(__always)
func _first<Source: AsyncSequence>(
  _ self: Source,
  where predicate: (Source.Element) async throws -> Bool
) async rethrows -> Source.Element? {
  for try await element in self {
    if try await predicate(element) {
      return element
    }
  }
  return nil
}

@available(SwiftStdlib 5.1, *)
extension AsyncSequence {
  /// Returns the first element of the sequence that satisfies the given
  /// predicate.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `10`. The `first(where:)` method returns the first
  /// member of the sequence that's evenly divisible by both `2` and `3`.
  ///
  ///     let divisibleBy2And3 = await Counter(howHigh: 10)
  ///         .first { $0 % 2 == 0 && $0 % 3 == 0 }
  ///     print(divisibleBy2And3 ?? "none")
  ///     // Prints "6"
  ///
  /// The predicate executes each time the asynchronous sequence produces an
  /// element, until either the predicate finds a match or the sequence ends.
  ///
  /// - Parameter predicate: A closure that takes an element of the asynchronous
  ///  sequence as its argument and returns a Boolean value that indicates
  ///  whether the element is a match.
  /// - Returns: The first element of the sequence that satisfies `predicate`,
  ///   or `nil` if there is no element that satisfies `predicate`.
  @inlinable
  public func first(
    where predicate: (Element) async throws -> Bool
  ) async rethrows -> Element? {
    return try await _first(self, where: predicate)
  }
}

@available(SwiftStdlib 5.1, *)
extension AsyncSequence {
  /// Returns the minimum element in the asynchronous sequence, using the given
  /// predicate as the comparison between elements.
  ///
  /// Use this method when the asynchronous sequence's values don't conform
  /// to `Comparable`, or when you want to apply a custom ordering to the
  /// sequence.
  ///
  /// The predicate must be a *strict weak ordering* over the elements. That is,
  /// for any elements `a`, `b`, and `c`, the following conditions must hold:
  ///
  /// - `areInIncreasingOrder(a, a)` is always `false`. (Irreflexivity)
  /// - If `areInIncreasingOrder(a, b)` and `areInIncreasingOrder(b, c)` are
  ///   both `true`, then `areInIncreasingOrder(a, c)` is also
  ///   `true`. (Transitive comparability)
  /// - Two elements are *incomparable* if neither is ordered before the other
  ///   according to the predicate. If `a` and `b` are incomparable, and `b`
  ///   and `c` are incomparable, then `a` and `c` are also incomparable.
  ///   (Transitive incomparability)
  ///
  /// The following example uses an enumeration of playing cards ranks, `Rank`,
  /// which ranges from `ace` (low) to `king` (high). An asynchronous sequence
  /// called `RankCounter` produces all elements of the array. The predicate
  /// provided to the `min(by:)` method sorts ranks based on their `rawValue`:
  ///
  ///     enum Rank: Int {
  ///         case ace = 1, two, three, four, five, six, seven, eight, nine, ten, jack, queen, king
  ///     }
  ///
  ///     let min = await RankCounter()
  ///         .min { $0.rawValue < $1.rawValue }
  ///     print(min ?? "none")
  ///     // Prints "ace"
  ///
  /// - Parameter areInIncreasingOrder: A predicate that returns `true` if its
  ///   first argument should be ordered before its second argument; otherwise,
  ///   `false`.
  /// - Returns: The sequence’s minimum element, according to
  ///   `areInIncreasingOrder`. If the sequence has no elements, returns `nil`.
  @inlinable
  @warn_unqualified_access
  public func min(
    by areInIncreasingOrder: (Element, Element) async throws -> Bool
  ) async rethrows -> Element? {
    var it = makeAsyncIterator()
    guard var result = try await it.next() else { 
      return nil 
    }
    while let e = try await it.next() {
      if try await areInIncreasingOrder(e, result) { 
        result = e 
      }
    }
    return result
  }
  
  /// Returns the maximum element in the asynchronous sequence, using the given
  /// predicate as the comparison between elements.
  ///
  /// Use this method when the asynchronous sequence's values don't conform
  /// to `Comparable`, or when you want to apply a custom ordering to the
  /// sequence.
  ///
  /// The predicate must be a *strict weak ordering* over the elements. That is,
  /// for any elements `a`, `b`, and `c`, the following conditions must hold:
  ///
  /// - `areInIncreasingOrder(a, a)` is always `false`. (Irreflexivity)
  /// - If `areInIncreasingOrder(a, b)` and `areInIncreasingOrder(b, c)` are
  ///   both `true`, then `areInIncreasingOrder(a, c)` is also
  ///   `true`. (Transitive comparability)
  /// - Two elements are *incomparable* if neither is ordered before the other
  ///   according to the predicate. If `a` and `b` are incomparable, and `b`
  ///   and `c` are incomparable, then `a` and `c` are also incomparable.
  ///   (Transitive incomparability)
  ///
  /// The following example uses an enumeration of playing cards ranks, `Rank`,
  /// which ranges from `ace` (low) to `king` (high). An asynchronous sequence
  /// called `RankCounter` produces all elements of the array. The predicate
  /// provided to the `max(by:)` method sorts ranks based on their `rawValue`:
  ///
  ///     enum Rank: Int {
  ///         case ace = 1, two, three, four, five, six, seven, eight, nine, ten, jack, queen, king
  ///     }
  ///
  ///     let max = await RankCounter()
  ///         .max { $0.rawValue < $1.rawValue }
  ///     print(max ?? "none")
  ///     // Prints "king"
  ///
  /// - Parameter areInIncreasingOrder: A predicate that returns `true` if its
  ///   first argument should be ordered before its second argument; otherwise,
  ///   `false`.
  /// - Returns: The sequence’s minimum element, according to
  ///   `areInIncreasingOrder`. If the sequence has no elements, returns `nil`.
  @inlinable
  @warn_unqualified_access
  public func max(
    by areInIncreasingOrder: (Element, Element) async throws -> Bool
  ) async rethrows -> Element? {
    var it = makeAsyncIterator()
    guard var result = try await it.next() else { 
      return nil 
    }
    while let e = try await it.next() {
      if try await areInIncreasingOrder(result, e) { 
        result = e 
      }
    }
    return result
  }
}

@available(SwiftStdlib 5.1, *)
extension AsyncSequence where Element: Comparable {
  /// Returns the minimum element in an asynchronous sequence of comparable
  /// elements.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `10`. The `min()` method returns the minimum value
  /// of the sequence.
  ///
  ///     let min = await Counter(howHigh: 10)
  ///         .min()
  ///     print(min ?? "none")
  ///     // Prints "1"
  ///
  /// - Returns: The sequence’s minimum element. If the sequence has no
  ///   elements, returns `nil`.
  @inlinable
  @warn_unqualified_access
  public func min() async rethrows -> Element? {
    return try await self.min(by: <)
  }

  /// Returns the maximum element in an asynchronous sequence of comparable
  /// elements.
  ///
  /// In this example, an asynchronous sequence called `Counter` produces `Int`
  /// values from `1` to `10`. The `max()` method returns the max value
  /// of the sequence.
  ///
  ///     let max = await Counter(howHigh: 10)
  ///         .max()
  ///     print(max ?? "none")
  ///     // Prints "10"
  ///
  /// - Returns: The sequence’s maximum element. If the sequence has no
  ///   elements, returns `nil`.
  @inlinable
  @warn_unqualified_access
  public func max() async rethrows -> Element? {
    return try await self.max(by: <)
  }
}

// These overloads exist to fix the incorrect overload resolution in an
// async context when a type conforms to both Sequence and AsyncSequence.
// They are now needed as Sequence methods are generalized for
// typed throws but AsyncSequence methods cannot be generalized yet.
//
// Without these overloads, the below now fails to compile:
// func f() async {
//   let ds = DualSequence()
//   _ = ds.contains { _ in false }
//       |- error: expression is 'async' but is not marked with 'await'
//       `- note: call is 'async'
// }
@available(SwiftStdlib 5.1, *)
extension AsyncSequence where Self: Sequence {
  /// Returns a Boolean value indicating whether the sequence contains an
  /// element that satisfies the given predicate.
  ///
  /// You can use the predicate to check for an element of a type that
  /// doesn't conform to the `Equatable` protocol, such as the
  /// `HTTPResponse` enumeration in this example.
  ///
  ///     enum HTTPResponse {
  ///         case ok
  ///         case error(Int)
  ///     }
  ///
  ///     let lastThreeResponses: [HTTPResponse] = [.ok, .ok, .error(404)]
  ///     let hadError = lastThreeResponses.contains { element in
  ///         if case .error = element {
  ///             return true
  ///         } else {
  ///             return false
  ///         }
  ///     }
  ///     // 'hadError' == true
  ///
  /// Alternatively, a predicate can be satisfied by a range of `Equatable`
  /// elements or a general condition. This example shows how you can check an
  /// array for an expense greater than $100.
  ///
  ///     let expenses = [21.37, 55.21, 9.32, 10.18, 388.77, 11.41]
  ///     let hasBigPurchase = expenses.contains { $0 > 100 }
  ///     // 'hasBigPurchase' == true
  ///
  /// - Parameter predicate: A closure that takes an element of the sequence
  ///   as its argument and returns a Boolean value that indicates whether
  ///   the passed element represents a match.
  /// - Returns: `true` if the sequence contains an element that satisfies
  ///   `predicate`; otherwise, `false`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @_alwaysEmitIntoClient
  public func contains(
    where predicate: (Element) throws -> Bool
  ) rethrows -> Bool {
    for e in self {
      if try predicate(e) {
        return true
      }
    }
    return false
  }

  /// Returns a Boolean value indicating whether every element of a sequence
  /// satisfies a given predicate.
  ///
  /// The following code uses this method to test whether all the names in an
  /// array have at least five characters:
  ///
  ///     let names = ["Sofia", "Camilla", "Martina", "Mateo", "Nicolás"]
  ///     let allHaveAtLeastFive = names.allSatisfy({ $0.count >= 5 })
  ///     // allHaveAtLeastFive == true
  ///
  /// If the sequence is empty, this method returns `true`.
  ///
  /// - Parameter predicate: A closure that takes an element of the sequence
  ///   as its argument and returns a Boolean value that indicates whether
  ///   the passed element satisfies a condition.
  /// - Returns: `true` if the sequence contains only elements that satisfy
  ///   `predicate`; otherwise, `false`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @_alwaysEmitIntoClient
  public func allSatisfy(
    _ predicate: (Element) throws -> Bool
  ) rethrows -> Bool {
    return try !contains { try !predicate($0) }
  }

  /// Returns the minimum element in the sequence, using the given predicate
  /// as the comparison between elements.
  ///
  /// The predicate must be a *strict weak ordering* over the elements. That
  /// is, for any elements `a`, `b`, and `c`, the following conditions must
  /// hold:
  ///
  /// - `areInIncreasingOrder(a, a)` is always `false`. (Irreflexivity)
  /// - If `areInIncreasingOrder(a, b)` and `areInIncreasingOrder(b, c)` are
  ///   both `true`, then `areInIncreasingOrder(a, c)` is also
  ///   `true`. (Transitive comparability)
  /// - Two elements are *incomparable* if neither is ordered before the other
  ///   according to the predicate. If `a` and `b` are incomparable, and `b`
  ///   and `c` are incomparable, then `a` and `c` are also incomparable.
  ///   (Transitive incomparability)
  ///
  /// This example shows how to use the `min(by:)` method on a
  /// dictionary to find the key-value pair with the lowest value.
  ///
  ///     let hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     let leastHue = hues.min { a, b in a.value < b.value }
  ///     print(leastHue)
  ///     // Prints "Optional((key: "Coral", value: 16))"
  ///
  /// - Parameter areInIncreasingOrder: A predicate that returns `true`
  ///   if its first argument should be ordered before its second
  ///   argument; otherwise, `false`.
  /// - Returns: The sequence's minimum element, according to
  ///   `areInIncreasingOrder`. If the sequence has no elements, returns
  ///   `nil`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @_alwaysEmitIntoClient
  @warn_unqualified_access
  public func min(
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows -> Element? {
    var it = makeIterator()
    guard var result = it.next() else { return nil }
    while let e = it.next() {
      if try areInIncreasingOrder(e, result) { result = e }
    }
    return result
  }

  /// Returns the maximum element in the sequence, using the given predicate
  /// as the comparison between elements.
  ///
  /// The predicate must be a *strict weak ordering* over the elements. That
  /// is, for any elements `a`, `b`, and `c`, the following conditions must
  /// hold:
  ///
  /// - `areInIncreasingOrder(a, a)` is always `false`. (Irreflexivity)
  /// - If `areInIncreasingOrder(a, b)` and `areInIncreasingOrder(b, c)` are
  ///   both `true`, then `areInIncreasingOrder(a, c)` is also
  ///   `true`. (Transitive comparability)
  /// - Two elements are *incomparable* if neither is ordered before the other
  ///   according to the predicate. If `a` and `b` are incomparable, and `b`
  ///   and `c` are incomparable, then `a` and `c` are also incomparable.
  ///   (Transitive incomparability)
  ///
  /// This example shows how to use the `max(by:)` method on a
  /// dictionary to find the key-value pair with the highest value.
  ///
  ///     let hues = ["Heliotrope": 296, "Coral": 16, "Aquamarine": 156]
  ///     let greatestHue = hues.max { a, b in a.value < b.value }
  ///     print(greatestHue)
  ///     // Prints "Optional((key: "Heliotrope", value: 296))"
  ///
  /// - Parameter areInIncreasingOrder:  A predicate that returns `true` if its
  ///   first argument should be ordered before its second argument;
  ///   otherwise, `false`.
  /// - Returns: The sequence's maximum element if the sequence is not empty;
  ///   otherwise, `nil`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @_alwaysEmitIntoClient
  @warn_unqualified_access
  public func max(
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows -> Element? {
    var it = makeIterator()
    guard var result = it.next() else { return nil }
    while let e = it.next() {
      if try areInIncreasingOrder(result, e) { result = e }
    }
    return result
  }

  /// Returns the result of combining the elements of the sequence using the
  /// given closure.
  ///
  /// Use the `reduce(_:_:)` method to produce a single value from the elements
  /// of an entire sequence. For example, you can use this method on an array
  /// of numbers to find their sum or product.
  ///
  /// The `nextPartialResult` closure is called sequentially with an
  /// accumulating value initialized to `initialResult` and each element of
  /// the sequence. This example shows how to find the sum of an array of
  /// numbers.
  ///
  ///     let numbers = [1, 2, 3, 4]
  ///     let numberSum = numbers.reduce(0, { x, y in
  ///         x + y
  ///     })
  ///     // numberSum == 10
  ///
  /// When `numbers.reduce(_:_:)` is called, the following steps occur:
  ///
  /// 1. The `nextPartialResult` closure is called with `initialResult`---`0`
  ///    in this case---and the first element of `numbers`, returning the sum:
  ///    `1`.
  /// 2. The closure is called again repeatedly with the previous call's return
  ///    value and each element of the sequence.
  /// 3. When the sequence is exhausted, the last value returned from the
  ///    closure is returned to the caller.
  ///
  /// If the sequence has no elements, `nextPartialResult` is never executed
  /// and `initialResult` is the result of the call to `reduce(_:_:)`.
  ///
  /// - Parameters:
  ///   - initialResult: The value to use as the initial accumulating value.
  ///     `initialResult` is passed to `nextPartialResult` the first time the
  ///     closure is executed.
  ///   - nextPartialResult: A closure that combines an accumulating value and
  ///     an element of the sequence into a new accumulating value, to be used
  ///     in the next call of the `nextPartialResult` closure or returned to
  ///     the caller.
  /// - Returns: The final accumulated value. If the sequence has no elements,
  ///   the result is `initialResult`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @_alwaysEmitIntoClient
  public func reduce<Result>(
    _ initialResult: Result,
    _ nextPartialResult:
      (_ partialResult: Result, Element) throws -> Result
  ) rethrows -> Result {
    var accumulator = initialResult
    for element in self {
      accumulator = try nextPartialResult(accumulator, element)
    }
    return accumulator
  }

  /// Returns the result of combining the elements of the sequence using the
  /// given closure.
  ///
  /// Use the `reduce(into:_:)` method to produce a single value from the
  /// elements of an entire sequence. For example, you can use this method on an
  /// array of integers to filter adjacent equal entries or count frequencies.
  ///
  /// This method is preferred over `reduce(_:_:)` for efficiency when the
  /// result is a copy-on-write type, for example an Array or a Dictionary.
  ///
  /// The `updateAccumulatingResult` closure is called sequentially with a
  /// mutable accumulating value initialized to `initialResult` and each element
  /// of the sequence. This example shows how to build a dictionary of letter
  /// frequencies of a string.
  ///
  ///     let letters = "abracadabra"
  ///     let letterCount = letters.reduce(into: [:]) { counts, letter in
  ///         counts[letter, default: 0] += 1
  ///     }
  ///     // letterCount == ["a": 5, "b": 2, "r": 2, "c": 1, "d": 1]
  ///
  /// When `letters.reduce(into:_:)` is called, the following steps occur:
  ///
  /// 1. The `updateAccumulatingResult` closure is called with the initial
  ///    accumulating value---`[:]` in this case---and the first character of
  ///    `letters`, modifying the accumulating value by setting `1` for the key
  ///    `"a"`.
  /// 2. The closure is called again repeatedly with the updated accumulating
  ///    value and each element of the sequence.
  /// 3. When the sequence is exhausted, the accumulating value is returned to
  ///    the caller.
  ///
  /// If the sequence has no elements, `updateAccumulatingResult` is never
  /// executed and `initialResult` is the result of the call to
  /// `reduce(into:_:)`.
  ///
  /// - Parameters:
  ///   - initialResult: The value to use as the initial accumulating value.
  ///   - updateAccumulatingResult: A closure that updates the accumulating
  ///     value with an element of the sequence.
  /// - Returns: The final accumulated value. If the sequence has no elements,
  ///   the result is `initialResult`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @_alwaysEmitIntoClient
  public func reduce<Result>(
    into initialResult: __owned Result,
    _ updateAccumulatingResult:
      (_ partialResult: inout Result, Element) throws -> ()
  ) rethrows -> Result {
    var accumulator = initialResult
    for element in self {
      try updateAccumulatingResult(&accumulator, element)
    }
    return accumulator
  }
}
