//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
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
/// values you can step through one at a time --- and adds asynchroncity. An
/// `AsyncSequence` may have all, some, or none of its values available when
/// you first use it. Instead, you use `await` to receive values as they become
/// available.
///
/// As with `Sequence`, you typically iterate through an `AsyncSequence` with a
/// `for`-`in` loop. However, since the caller must potentially wait for values,
/// you use the `await` keyword. The following example shows how to iterate
/// over `Counter`, a custom `AsyncSequence` that produces `Int` values from
/// `1` up to a `howHigh` value:
///
///     for await i in Counter(howHigh: 10) {
///       print(i, terminator: " ")
///     }
///     // Prints: 1 2 3 4 5 6 7 8 9 10
///
/// An `AsyncSequence` doesn't generate or contain the values; it just defines
/// how you access them. Along with defining the type of values as an associated
/// type called `Element`, the `AsyncSequence` defines a `makeAsyncIterator()`
/// method. This returns an instance of type `AsyncIterator`. Like the standard
/// `IteratorProtocol`, the `AsyncIterator` defines a single `next()` method
/// to produce elements. The difference is that the `AsyncIterator` defines its
/// `next()` method as `async`, which a caller to `await` the next value.
///
/// `AsyncSequence` also defines functions that allow you to perform common
/// processing of the elements you receive, modeled on the operations provided
/// by the basic `Sequence` in the standard library. There are two categories of
/// functions: those that return a single value, and those that return another
/// `AsyncSequence`.
///
/// Single-value functions eliminate the need for a `for`-`in` loop, and instead
/// let you make a single `await` call. For example, the `contains(_:)` method
/// returns a Boolean value that indicates if a given value exists in the
/// `AsyncSequence`. Given the `Counter` sequence from the previous example,
/// you can test for the existence of a sequence member with a one-line call:
///
///     let found = try await Counter(howHigh: 10).contains(5) // true
///
/// Functions that return another `AsyncSequence` return a type specific to the
/// function's semantics. For example, the `.map(_:)` operator returns a
/// `AsyncMapSequence` (or a `AsyncThrowingMapSequence`, if the closure you
/// provide to the `map(_:)` function can throw an error). These returned
/// sequences don't eagerly await the next member of the sequence, which allows
/// the caller to decide when to start work. Typically, you'll iterate over
/// these sequences with `for`-`in`, like the base `AsyncSequence` you started
/// with. In the following example, the `map(_:)` function transforms each `Int`
/// received from a `Counter` sequence into a `String`:
///
///     for await s in Counter(howHigh: 10)
///       .map( { $0 % 2 == 0 ? "Even" : "Odd" } ) {
///         print(s, terminator: " ")
///     }
///     // Prints: Odd Even Odd Even Odd Even Odd Even Odd Even
///
@available(SwiftStdlib 5.5, *)
@rethrows
public protocol AsyncSequence {
  associatedtype AsyncIterator: AsyncIteratorProtocol where AsyncIterator.Element == Element
  associatedtype Element
  __consuming func makeAsyncIterator() -> AsyncIterator
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
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

@available(SwiftStdlib 5.5, *)
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

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
  @inlinable
  public func contains(
    where predicate: (Element) async throws -> Bool
  ) async rethrows -> Bool {
    return try await _contains(self, where: predicate)
  }

  @inlinable
  public func allSatisfy(
    _ predicate: (Element) async throws -> Bool
  ) async rethrows -> Bool {
    return try await !contains { try await !predicate($0) }
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence where Element: Equatable {
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

@available(SwiftStdlib 5.5, *)
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

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
  @inlinable
  public func first(
    where predicate: (Element) async throws -> Bool
  ) async rethrows -> Element? {
    return try await _first(self, where: predicate)
  }
}

@available(SwiftStdlib 5.5, *)
extension AsyncSequence {
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

@available(SwiftStdlib 5.5, *)
extension AsyncSequence where Element: Comparable {
  @inlinable
  @warn_unqualified_access
  public func min() async rethrows -> Element? {
    return try await self.min(by: <)
  }

  @inlinable
  @warn_unqualified_access
  public func max() async rethrows -> Element? {
    return try await self.max(by: <)
  }
}
