//===--- BorrowingSequenceAlgorithms.swift --------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// starts(with:)
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  /// Returns a Boolean value indicating whether the initial elements of the
  /// sequence are equivalent to the elements in another sequence, using
  /// the given predicate as the equivalence test.
  ///
  /// The predicate must be an *equivalence relation* over the elements. That
  /// is, for any elements `a`, `b`, and `c`, the following conditions must
  /// hold:
  ///
  /// - `areEquivalent(a, a)` is always `true`. (Reflexivity)
  /// - `areEquivalent(a, b)` implies `areEquivalent(b, a)`. (Symmetry)
  /// - If `areEquivalent(a, b)` and `areEquivalent(b, c)` are both `true`, then
  ///   `areEquivalent(a, c)` is also `true`. (Transitivity)
  ///
  /// - Parameters:
  ///   - possiblePrefix: A sequence to compare to this sequence.
  ///   - areEquivalent: A predicate that returns `true` if its two arguments
  ///     are equivalent; otherwise, `false`.
  /// - Returns: `true` if the initial elements of the sequence are equivalent
  ///   to the elements of `possiblePrefix`; otherwise, `false`. If
  ///   `possiblePrefix` has no elements, the return value is `true`.
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `possiblePrefix`.
  @inlinable
  public func starts<PossiblePrefix: BorrowingSequence>(
    with possiblePrefix: borrowing PossiblePrefix,
    by areEquivalent: (borrowing Element, borrowing PossiblePrefix.Element) throws -> Bool
  ) rethrows -> Bool where PossiblePrefix: ~Copyable & ~Escapable, PossiblePrefix.Element: ~Copyable {
    var iter1 = makeBorrowingIterator()
    var iter2 = possiblePrefix.makeBorrowingIterator()
    while true {
      var el1 = iter1.nextSpan(maximumCount: .max)

      if el1.isEmpty {
        // LHS is empty - starts(with:) is true iff RHS is also empty
        let el2 = iter2.nextSpan(maximumCount: 1)
        return el2.isEmpty
      }

      while el1.count > 0 {
        let el2 = iter2.nextSpan(maximumCount: el1.count)
        if el2.isEmpty {
          // Reached end of RHS without a mismatch
          return true
        }
        for i in 0..<el2.count {
          if try !areEquivalent(el1[i], el2[i]) { return false }
        }
        el1 = el1.extracting(droppingFirst: el2.count)
      }
    }
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable & Equatable {
  /// Returns a Boolean value indicating whether the initial elements of the
  /// sequence are the same as the elements in another sequence.
  ///
  /// This example tests whether one countable range begins with the elements
  /// of another countable range.
  ///
  ///     let a = 1...3
  ///     let b = 1...10
  ///
  ///     print(b.starts(with: a))
  ///     // Prints "true"
  ///
  /// Passing a sequence with no elements or an empty collection as
  /// `possiblePrefix` always results in `true`.
  ///
  ///     print(b.starts(with: []))
  ///     // Prints "true"
  ///
  /// - Parameter possiblePrefix: A sequence to compare to this sequence.
  /// - Returns: `true` if the initial elements of the sequence are the same as
  ///   the elements of `possiblePrefix`; otherwise, `false`. If
  ///   `possiblePrefix` has no elements, the return value is `true`.
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `possiblePrefix`.
  @inlinable
  public func starts<PossiblePrefix: BorrowingSequence<Element>>(
    with possiblePrefix: borrowing PossiblePrefix,
  ) -> Bool where PossiblePrefix: ~Copyable & ~Escapable {
    return self.starts(with: possiblePrefix, by: ==)
  }
}

//===----------------------------------------------------------------------===//
// elementsEqual()
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  /// Returns a Boolean value indicating whether this sequence and another
  /// sequence contain equivalent elements in the same order, using the given
  /// predicate as the equivalence test.
  ///
  /// At least one of the sequences must be finite.
  ///
  /// The predicate must be an *equivalence relation* over the elements. That
  /// is, for any elements `a`, `b`, and `c`, the following conditions must
  /// hold:
  ///
  /// - `areEquivalent(a, a)` is always `true`. (Reflexivity)
  /// - `areEquivalent(a, b)` implies `areEquivalent(b, a)`. (Symmetry)
  /// - If `areEquivalent(a, b)` and `areEquivalent(b, c)` are both `true`, then
  ///   `areEquivalent(a, c)` is also `true`. (Transitivity)
  ///
  /// - Parameters:
  ///   - other: A sequence to compare to this sequence.
  ///   - areEquivalent: A predicate that returns `true` if its two arguments
  ///     are equivalent; otherwise, `false`.
  /// - Returns: `true` if this sequence and `other` contain equivalent items,
  ///   using `areEquivalent` as the equivalence test; otherwise, `false.`
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @inlinable
  public func elementsEqual<OtherSequence: BorrowingSequence>(
    _ other: borrowing OtherSequence,
    by areEquivalent: (borrowing Element, borrowing OtherSequence.Element) throws -> Bool
  ) rethrows -> Bool where OtherSequence: ~Copyable & ~Escapable, OtherSequence.Element: ~Copyable {
    var iter1 = makeBorrowingIterator()
    var iter2 = other.makeBorrowingIterator()
    while true {
      var el1 = iter1.nextSpan(maximumCount: .max)

      if el1.isEmpty {
        // LHS is empty - sequences are equal iff RHS is also empty
        let el2 = iter2.nextSpan(maximumCount: 1)
        return el2.isEmpty
      }

      while el1.count > 0 {
        let el2 = iter2.nextSpan(maximumCount: el1.count)
        if el2.isEmpty { return false }
        for i in 0..<el2.count {
          if try !areEquivalent(el1[i], el2[i]) { return false }
        }
        el1 = el1.extracting(droppingFirst: el2.count)
      }
    }
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable & Equatable {
  /// Returns a Boolean value indicating whether this sequence and another
  /// sequence contain the same elements in the same order.
  ///
  /// At least one of the sequences must be finite.
  ///
  /// This example tests whether one countable range shares the same elements
  /// as another countable range and an array.
  ///
  ///     let a = 1...3
  ///     let b = 1...10
  ///
  ///     print(a.elementsEqual(b))
  ///     // Prints "false"
  ///     print(a.elementsEqual([1, 2, 3]))
  ///     // Prints "true"
  ///
  /// - Parameter other: A sequence to compare to this sequence.
  /// - Returns: `true` if this sequence and `other` contain the same elements
  ///   in the same order.
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @inlinable
  public func elementsEqual<OtherSequence: BorrowingSequence<Element>>(
    _ other: borrowing OtherSequence
  ) -> Bool where OtherSequence: ~Copyable & ~Escapable, OtherSequence.Element: ~Copyable {
    // FIXME: 'OtherSequence.Element: ~Copyable' shouldn't be necessary
    return self.elementsEqual(other, by: ==)
  }
}

//===----------------------------------------------------------------------===//
// lexicographicallyPrecedes()
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  /// Returns a Boolean value indicating whether the sequence precedes another
  /// sequence in a lexicographical (dictionary) ordering, using the given
  /// predicate to compare elements.
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
  /// - Parameters:
  ///   - other: A sequence to compare to this sequence.
  ///   - areInIncreasingOrder:  A predicate that returns `true` if its first
  ///     argument should be ordered before its second argument; otherwise,
  ///     `false`.
  /// - Returns: `true` if this sequence precedes `other` in a dictionary
  ///   ordering as ordered by `areInIncreasingOrder`; otherwise, `false`.
  ///
  /// - Note: This method implements the mathematical notion of lexicographical
  ///   ordering, which has no connection to Unicode.  If you are sorting
  ///   strings to present to the end user, use `String` APIs that perform
  ///   localized comparison instead.
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @inlinable
  public func lexicographicallyPrecedes<OtherSequence: BorrowingSequence<Element>>(
    _ other: borrowing OtherSequence,
    by areInIncreasingOrder: (borrowing Element, borrowing Element) throws -> Bool
  ) rethrows -> Bool where OtherSequence: ~Copyable & ~Escapable {
    var iter1 = makeBorrowingIterator()
    var iter2 = other.makeBorrowingIterator()
    while true {
      var el1 = iter1.nextSpan(maximumCount: .max)

      if el1.isEmpty {
        return true
      }

      while el1.count > 0 {
        let el2 = iter2.nextSpan(maximumCount: el1.count)
        if el2.isEmpty { return false }
        for i in 0..<el2.count {
          if try areInIncreasingOrder(el1[i], el2[i]) {
            return true
          }
          if try areInIncreasingOrder(el2[i], el1[i]) {
            return false
          }
          // Items are equivalent; keep going
        }
        el1 = el1.extracting(droppingFirst: el2.count)
      }
    }
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable & Comparable {
  /// Returns a Boolean value indicating whether the sequence precedes another
  /// sequence in a lexicographical (dictionary) ordering, using the
  /// less-than operator (`<`) to compare elements.
  ///
  /// This example uses the `lexicographicallyPrecedes` method to test which
  /// array of integers comes first in a lexicographical ordering.
  ///
  ///     let a = [1, 2, 2, 2]
  ///     let b = [1, 2, 3, 4]
  ///
  ///     print(a.lexicographicallyPrecedes(b))
  ///     // Prints "true"
  ///     print(b.lexicographicallyPrecedes(b))
  ///     // Prints "false"
  ///
  /// - Parameter other: A sequence to compare to this sequence.
  /// - Returns: `true` if this sequence precedes `other` in a dictionary
  ///   ordering; otherwise, `false`.
  ///
  /// - Note: This method implements the mathematical notion of lexicographical
  ///   ordering, which has no connection to Unicode.  If you are sorting
  ///   strings to present to the end user, use `String` APIs that
  ///   perform localized comparison.
  ///
  /// - Complexity: O(*m*), where *m* is the lesser of the length of the
  ///   sequence and the length of `other`.
  @inlinable
  public func lexicographicallyPrecedes<OtherSequence: BorrowingSequence<Element>>(
    _ other: borrowing OtherSequence
  ) -> Bool where OtherSequence: ~Copyable & ~Escapable {
    return self.lexicographicallyPrecedes(other, by: <)
  }
}

//===----------------------------------------------------------------------===//
// contains()
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable {
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
  @inlinable
  public func contains(
    where predicate: (borrowing Element) throws -> Bool
  ) rethrows -> Bool {
    var iterator = makeBorrowingIterator()
    while true {
      var span = iterator.nextSpan(maximumCount: .max)
      if span.isEmpty {
        return false
      }
      
      for i in span.indices {
        if try predicate(span[i]) {
          return true
        }
      }
    }
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
  @inlinable
  public func allSatisfy(
    _ predicate: (borrowing Element) throws -> Bool
  ) rethrows -> Bool {
    return try !contains { try !predicate($0) }
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable & Equatable {
  /// Returns a Boolean value indicating whether the sequence contains the
  /// given element.
  ///
  /// This example checks to see whether a favorite actor is in an array
  /// storing a movie's cast.
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     print(cast.contains("Marlon"))
  ///     // Prints "true"
  ///     print(cast.contains("James"))
  ///     // Prints "false"
  ///
  /// - Parameter element: The element to find in the sequence.
  /// - Returns: `true` if the element was found in the sequence; otherwise,
  ///   `false`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence.
  @inlinable
  public func contains(_ element: borrowing Element) -> Bool {
    if let result = _customContainsEquatableElement(element) {
      return result
    } else {
      return self.contains { $0 == element }
    }
  }
}

//===----------------------------------------------------------------------===//
// count(where:)
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  /// Returns the number of elements in the sequence that satisfy the given
  /// predicate.
  ///
  /// You can use this method to count the number of elements that pass a test.
  /// The following example finds the number of names that are fewer than
  /// five characters long:
  ///
  ///     let names = ["Jacqueline", "Ian", "Amy", "Juan", "Soroush", "Tiffany"]
  ///     let shortNameCount = names.count(where: { $0.count < 5 })
  ///     // shortNameCount == 3
  ///
  /// To find the number of times a specific element appears in the sequence,
  /// use the equal to operator (`==`) in the closure to test for a match.
  ///
  ///     let birds = ["duck", "duck", "duck", "duck", "goose"]
  ///     let duckCount = birds.count(where: { $0 == "duck" })
  ///     // duckCount == 4
  ///
  /// The sequence must be finite.
  ///
  /// - Parameter predicate: A closure that takes each element of the sequence
  ///   as its argument and returns a Boolean value indicating whether
  ///   the element should be included in the count.
  /// - Returns: The number of elements in the sequence that satisfy the given
  ///   predicate.
  @_alwaysEmitIntoClient
  public func count<E>(
    where predicate: (borrowing Element) throws(E) -> Bool
  ) throws(E) -> Int {
    var iterator = makeBorrowingIterator()
    var count = 0
    while true {
      var span = iterator.nextSpan(maximumCount: .max)
      if span.isEmpty {
        return count
      }
      
      for i in span.indices {
        if try predicate(span[i]) {
          count += try predicate(span[i]) ? 1 : 0
        }
      }
    }
  }
}

//===----------------------------------------------------------------------===//
// reduce()
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable {
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
  @inlinable
  public func reduce<Result: ~Copyable>(
    _ initialResult: consuming Result,
    _ nextPartialResult:
      (_ partialResult: consuming Result, borrowing Element) throws -> Result
  ) rethrows -> Result {
    var iterator = makeBorrowingIterator()
    var accumulator = initialResult
    while true {
      var span = iterator.nextSpan(maximumCount: .max)
      if span.isEmpty {
        return accumulator
      }
      
      for i in span.indices {
        accumulator = try nextPartialResult(accumulator, span[i])
      }
    }
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
  @inlinable
  public func reduce<Result>(
    into initialResult: consuming Result,
    _ updateAccumulatingResult:
      (_ partialResult: inout Result, borrowing Element) throws -> ()
  ) rethrows -> Result {
    var iterator = makeBorrowingIterator()
    var accumulator = initialResult
    while true {
      var span = iterator.nextSpan(maximumCount: .max)
      if span.isEmpty {
        return accumulator
      }
      
      for i in span.indices {
        try updateAccumulatingResult(&accumulator, span[i])
      }
    }
  }
}


@available(SwiftStdlib 6.4, *)
public struct PrefixWhileBorrowingIterator<Base: BorrowingIteratorProtocol>: BorrowingIteratorProtocol, ~Copyable, ~Escapable
  where Base: ~Copyable & ~Escapable, Base.Element: ~Copyable
{
  public typealias Element = Base.Element
  
  var base: Base
  var predicate: (borrowing Base.Element) -> Bool
  var foundNonMatchingElement: Bool = false

  @_lifetime(copy base)
  init(_ base: consuming Base, predicate: @escaping (borrowing Base.Element) -> Bool) {
    self.base = base
    self.predicate = predicate
  }
  
  @_lifetime(&self)
  public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
    if foundNonMatchingElement {
      return Span()
    }
    
    let span = base.nextSpan(maximumCount: maximumCount)
    for i in span.indices {
      if !predicate(span[i]) {
        foundNonMatchingElement = true
        return span.extracting(first: i)
      }
    }
    return span
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingIteratorProtocol where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  @_lifetime(copy self)
  public consuming func prefix(
    while predicate: @escaping (borrowing Element) -> Bool
  ) -> PrefixWhileBorrowingIterator<Self> {
    .init(self, predicate: predicate)
  }
}

@available(SwiftStdlib 6.4, *)
extension BorrowingSequence where Self: ~Copyable & ~Escapable, Element: ~Copyable {
  @_lifetime(borrow self)
  public func prefix(
    while predicate: @escaping (borrowing Element) -> Bool
  ) -> PrefixWhileBorrowingIterator<Self.BorrowingIterator> {
    makeBorrowingIterator().prefix(while: predicate)
  }
}


//
//
//@available(SwiftStdlib 6.4, *)
//public struct PrefixWhileBorrowingSequence<Base: BorrowingSequence>: BorrowingSequence, ~Copyable, ~Escapable
//  where Base: ~Copyable /* & ~Escapable */, Base.Element: ~Copyable
//{
//  public typealias Element = Base.Element
//  
//  var base: Borrow<Base>
//  var predicate: (borrowing Base.Element) -> Bool
//  
//  @_lifetime(borrow base)
//  init(_ base: borrowing Base, predicate: @escaping (borrowing Base.Element) -> Bool) {
//    self.base = Borrow(base)
//    self.predicate = predicate
//  }
//  
//  @_lifetime(borrow self)
//  public func makeBorrowingIterator() -> BorrowingIterator {
//    BorrowingIterator(base: base.value, iterator: base.value.makeBorrowingIterator(), predicate: predicate)
//  }
//  
//  public struct BorrowingIterator: BorrowingIteratorProtocol, ~Copyable, ~Escapable {
//    var iterator: Base.BorrowingIterator
//    var predicate: (borrowing Base.Element) -> Bool
//    var foundNonMatchingElement: Bool = false
//    
//    @_lifetime(borrow base)
//    init(base: borrowing Base, iterator: consuming Base.BorrowingIterator, predicate: @escaping (borrowing Base.Element) -> Bool) {
//      self.iterator = iterator
//      self.predicate = predicate
//    }
//
//    @_lifetime(&self)
//    public mutating func nextSpan(maximumCount: Int) -> Span<Element> {
//      if foundNonMatchingElement {
//        return Span()
//      }
//      
//      let span = iterator.nextSpan(maximumCount: maximumCount)
//      for i in span.indices {
//        if !predicate(span[i]) {
//          foundNonMatchingElement = true
//          return span.extracting(first: i)
//        }
//      }
//      return span
//    }
//  }
//}
//
//@available(SwiftStdlib 6.4, *)
//extension BorrowingSequence where Self: ~Copyable /* & ~Escapable */, Element: ~Copyable {
//  @_lifetime(borrow self)
//  public func prefix(while predicate: @escaping (borrowing Element) -> Bool) -> PrefixWhileBorrowingSequence<Self> {
//    .init(self, predicate: predicate)
//  }
//}
