//===--- CollectionAlgorithms.swift.gyb -----------------------*- swift -*-===//
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
// last
//===----------------------------------------------------------------------===//

extension BidirectionalCollection {
  /// The last element of the collection.
  ///
  /// If the collection is empty, the value of this property is `nil`.
  ///
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     if let lastNumber = numbers.last {
  ///         print(lastNumber)
  ///     }
  ///     // Prints "50"
  @inlinable
  public var last: Element? {
    return isEmpty ? nil : self[index(before: endIndex)]
  }
}

//===----------------------------------------------------------------------===//
// firstIndex(of:)/firstIndex(where:)
//===----------------------------------------------------------------------===//

extension Collection where Element : Equatable {
  /// Returns the first index where the specified value appears in the
  /// collection.
  ///
  /// After using `firstIndex(of:)` to find the position of a particular element
  /// in a collection, you can use it to access the element by subscripting.
  /// This example shows how you can modify one of the names in an array of
  /// students.
  ///
  ///     var students = ["Ben", "Ivy", "Jordell", "Maxime"]
  ///     if let i = students.firstIndex(of: "Maxime") {
  ///         students[i] = "Max"
  ///     }
  ///     print(students)
  ///     // Prints "["Ben", "Ivy", "Jordell", "Max"]"
  ///
  /// - Parameter element: An element to search for in the collection.
  /// - Returns: The first index where `element` is found. If `element` is not
  ///   found in the collection, returns `nil`.
  @inlinable
  public func firstIndex(of element: Element) -> Index? {
    if let result = _customIndexOfEquatableElement(element) {
      return result
    }

    var i = self.startIndex
    while i != self.endIndex {
      if self[i] == element {
        return i
      }
      self.formIndex(after: &i)
    }
    return nil
  }
  
  /// Returns the first index where the specified value appears in the
  /// collection.
  @inlinable
  public func index(of _element: Element) -> Index? {
    return firstIndex(of: _element)
  }
}

extension Collection {
  /// Returns the first index in which an element of the collection satisfies
  /// the given predicate.
  ///
  /// You can use the predicate to find an element of a type that doesn't
  /// conform to the `Equatable` protocol or to find an element that matches
  /// particular criteria. Here's an example that finds a student name that
  /// begins with the letter "A":
  ///
  ///     let students = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
  ///     if let i = students.firstIndex(where: { $0.hasPrefix("A") }) {
  ///         print("\(students[i]) starts with 'A'!")
  ///     }
  ///     // Prints "Abena starts with 'A'!"
  ///
  /// - Parameter predicate: A closure that takes an element as its argument
  ///   and returns a Boolean value that indicates whether the passed element
  ///   represents a match.
  /// - Returns: The index of the first element for which `predicate` returns
  ///   `true`. If no elements in the collection satisfy the given predicate,
  ///   returns `nil`.
  @inlinable
  public func firstIndex(
    where predicate: (Element) throws -> Bool
  ) rethrows -> Index? {
    var i = self.startIndex
    while i != self.endIndex {
      if try predicate(self[i]) {
        return i
      }
      self.formIndex(after: &i)
    }
    return nil
  }
  
  /// Returns the first index in which an element of the collection satisfies
  /// the given predicate.
  @inlinable
  public func index(
    where _predicate: (Element) throws -> Bool
  ) rethrows -> Index? {
    return try firstIndex(where: _predicate)
  }
}

//===----------------------------------------------------------------------===//
// lastIndex(of:)/lastIndex(where:)
//===----------------------------------------------------------------------===//

extension BidirectionalCollection {
  /// Returns the last element of the sequence that satisfies the given
  /// predicate.
  ///
  /// This example uses the `last(where:)` method to find the last
  /// negative number in an array of integers:
  ///
  ///     let numbers = [3, 7, 4, -2, 9, -6, 10, 1]
  ///     if let lastNegative = numbers.last(where: { $0 < 0 }) {
  ///         print("The last negative number is \(firstNegative).")
  ///     }
  ///     // Prints "The last negative number is -6."
  ///
  /// - Parameter predicate: A closure that takes an element of the sequence as
  ///   its argument and returns a Boolean value indicating whether the
  ///   element is a match.
  /// - Returns: The last element of the sequence that satisfies `predicate`,
  ///   or `nil` if there is no element that satisfies `predicate`.
  @inlinable
  public func last(
    where predicate: (Element) throws -> Bool
  ) rethrows -> Element? {
    return try lastIndex(where: predicate).map { self[$0] }
  }

  /// Returns the index of the last element in the collection that matches the
  /// given predicate.
  ///
  /// You can use the predicate to find an element of a type that doesn't
  /// conform to the `Equatable` protocol or to find an element that matches
  /// particular criteria. This example finds the index of the last name that
  /// begins with the letter "A":
  ///
  ///     let students = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
  ///     if let i = students.lastIndex(where: { $0.hasPrefix("A") }) {
  ///         print("\(students[i]) starts with 'A'!")
  ///     }
  ///     // Prints "Akosua starts with 'A'!"
  ///
  /// - Parameter predicate: A closure that takes an element as its argument
  ///   and returns a Boolean value that indicates whether the passed element
  ///   represents a match.
  /// - Returns: The index of the last element in the collection that matches
  ///   `predicate`, or `nil` if no elements match.
  @inlinable
  public func lastIndex(
    where predicate: (Element) throws -> Bool
  ) rethrows -> Index? {
    var i = endIndex
    while i != startIndex {
      formIndex(before: &i)
      if try predicate(self[i]) {
        return i
      }
    }
    return nil
  }
}

extension BidirectionalCollection where Element : Equatable {
  /// Returns the last index where the specified value appears in the
  /// collection.
  ///
  /// After using `lastIndex(of:)` to find the position of the last instance of
  /// a particular element in a collection, you can use it to access the
  /// element by subscripting. This example shows how you can modify one of
  /// the names in an array of students.
  ///
  ///     var students = ["Ben", "Ivy", "Jordell", "Ben", "Maxime"]
  ///     if let i = students.lastIndex(of: "Ben") {
  ///         students[i] = "Benjamin"
  ///     }
  ///     print(students)
  ///     // Prints "["Ben", "Ivy", "Jordell", "Benjamin", "Max"]"
  ///
  /// - Parameter element: An element to search for in the collection.
  /// - Returns: The last index where `element` is found. If `element` is not
  ///   found in the collection, returns `nil`.
  @inlinable
  public func lastIndex(of element: Element) -> Index? {
    if let result = _customLastIndexOfEquatableElement(element) {
      return result
    }
    return lastIndex(where: { $0 == element })
  }
}

//===----------------------------------------------------------------------===//
// partition(by:)
//===----------------------------------------------------------------------===//

extension MutableCollection {
  /// Reorders the elements of the collection such that all the elements
  /// that match the given predicate are after all the elements that don't
  /// match.
  ///
  /// After partitioning a collection, there is a pivot index `p` where
  /// no element before `p` satisfies the `belongsInSecondPartition`
  /// predicate and every element at or after `p` satisfies
  /// `belongsInSecondPartition`.
  ///
  /// In the following example, an array of numbers is partitioned by a
  /// predicate that matches elements greater than 30.
  ///
  ///     var numbers = [30, 40, 20, 30, 30, 60, 10]
  ///     let p = numbers.partition(by: { $0 > 30 })
  ///     // p == 5
  ///     // numbers == [30, 10, 20, 30, 30, 60, 40]
  ///
  /// The `numbers` array is now arranged in two partitions. The first
  /// partition, `numbers[..<p]`, is made up of the elements that
  /// are not greater than 30. The second partition, `numbers[p...]`,
  /// is made up of the elements that *are* greater than 30.
  ///
  ///     let first = numbers[..<p]
  ///     // first == [30, 10, 20, 30, 30]
  ///     let second = numbers[p...]
  ///     // second == [60, 40]
  ///
  /// - Parameter belongsInSecondPartition: A predicate used to partition
  ///   the collection. All elements satisfying this predicate are ordered
  ///   after all elements not satisfying it.
  /// - Returns: The index of the first element in the reordered collection
  ///   that matches `belongsInSecondPartition`. If no elements in the
  ///   collection match `belongsInSecondPartition`, the returned index is
  ///   equal to the collection's `endIndex`.
  ///
  /// - Complexity: O(*n*)
  @inlinable
  public mutating func partition(
    by belongsInSecondPartition: (Element) throws -> Bool
  ) rethrows -> Index {

    var pivot = startIndex
    while true {
      if pivot == endIndex {
        return pivot
      }
      if try belongsInSecondPartition(self[pivot]) {
        break
      }
      formIndex(after: &pivot)
    }

    var i = index(after: pivot)
    while i < endIndex {
      if try !belongsInSecondPartition(self[i]) {
        swapAt(i, pivot)
        formIndex(after: &pivot)
      }
      formIndex(after: &i)
    }
    return pivot
  }
}

extension MutableCollection where Self : BidirectionalCollection {
  /// Reorders the elements of the collection such that all the elements
  /// that match the given predicate are after all the elements that don't
  /// match.
  ///
  /// After partitioning a collection, there is a pivot index `p` where
  /// no element before `p` satisfies the `belongsInSecondPartition`
  /// predicate and every element at or after `p` satisfies
  /// `belongsInSecondPartition`.
  ///
  /// In the following example, an array of numbers is partitioned by a
  /// predicate that matches elements greater than 30.
  ///
  ///     var numbers = [30, 40, 20, 30, 30, 60, 10]
  ///     let p = numbers.partition(by: { $0 > 30 })
  ///     // p == 5
  ///     // numbers == [30, 10, 20, 30, 30, 60, 40]
  ///
  /// The `numbers` array is now arranged in two partitions. The first
  /// partition, `numbers[..<p]`, is made up of the elements that
  /// are not greater than 30. The second partition, `numbers[p...]`,
  /// is made up of the elements that *are* greater than 30.
  ///
  ///     let first = numbers[..<p]
  ///     // first == [30, 10, 20, 30, 30]
  ///     let second = numbers[p...]
  ///     // second == [60, 40]
  ///
  /// - Parameter belongsInSecondPartition: A predicate used to partition
  ///   the collection. All elements satisfying this predicate are ordered
  ///   after all elements not satisfying it.
  /// - Returns: The index of the first element in the reordered collection
  ///   that matches `belongsInSecondPartition`. If no elements in the
  ///   collection match `belongsInSecondPartition`, the returned index is
  ///   equal to the collection's `endIndex`.
  ///
  /// - Complexity: O(*n*)
  @inlinable
  public mutating func partition(
    by belongsInSecondPartition: (Element) throws -> Bool
  ) rethrows -> Index {
    let maybeOffset = try _withUnsafeMutableBufferPointerIfSupported {
      (bufferPointer) -> Int in
      let unsafeBufferPivot = try bufferPointer.partition(
        by: belongsInSecondPartition)
      return unsafeBufferPivot - bufferPointer.startIndex
    }
    if let offset = maybeOffset {
      return index(startIndex, offsetBy: numericCast(offset))
    }

    var lo = startIndex
    var hi = endIndex

    // 'Loop' invariants (at start of Loop, all are true):
    // * lo < hi
    // * predicate(self[i]) == false, for i in startIndex ..< lo
    // * predicate(self[i]) == true, for i in hi ..< endIndex

    Loop: while true {
      FindLo: repeat {
        while lo < hi {
          if try belongsInSecondPartition(self[lo]) { break FindLo }
          formIndex(after: &lo)
        }
        break Loop
      } while false

      FindHi: repeat {
        formIndex(before: &hi)
        while lo < hi {
          if try !belongsInSecondPartition(self[hi]) { break FindHi }
          formIndex(before: &hi)
        }
        break Loop
      } while false

      swapAt(lo, hi)
      formIndex(after: &lo)
    }

    return lo
  }
}

//===----------------------------------------------------------------------===//
// sorted()/sort()
//===----------------------------------------------------------------------===//

extension Sequence where Element : Comparable {
  /// Returns the elements of the sequence, sorted.
  ///
  /// You can sort any sequence of elements that conform to the `Comparable`
  /// protocol by calling this method. Elements are sorted in ascending order.
  ///
  /// The sorting algorithm is not stable. A nonstable sort may change the
  /// relative order of elements that compare equal.
  ///
  /// Here's an example of sorting a list of students' names. Strings in Swift
  /// conform to the `Comparable` protocol, so the names are sorted in
  /// ascending order according to the less-than operator (`<`).
  ///
  ///     let students: Set = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
  ///     let sortedStudents = students.sorted()
  ///     print(sortedStudents)
  ///     // Prints "["Abena", "Akosua", "Kofi", "Kweku", "Peter"]"
  ///
  /// To sort the elements of your sequence in descending order, pass the
  /// greater-than operator (`>`) to the `sorted(by:)` method.
  ///
  ///     let descendingStudents = students.sorted(by: >)
  ///     print(descendingStudents)
  ///     // Prints "["Peter", "Kweku", "Kofi", "Akosua", "Abena"]"
  ///
  /// - Returns: A sorted array of the sequence's elements.
  @inlinable
  public func sorted() -> [Element] {
    var result = ContiguousArray(self)
    result.sort()
    return Array(result)
  }
}

extension Sequence {
  /// Returns the elements of the sequence, sorted using the given predicate as
  /// the comparison between elements.
  ///
  /// When you want to sort a sequence of elements that don't conform to the
  /// `Comparable` protocol, pass a predicate to this method that returns
  /// `true` when the first element passed should be ordered before the
  /// second. The elements of the resulting array are ordered according to the
  /// given predicate.
  ///
  /// The predicate must be a *strict weak ordering* over the elements. That
  /// is, for any elements `a`, `b`, and `c`, the following conditions must
  /// hold:
  ///
  /// - `areInIncreasingOrder(a, a)` is always `false`. (Irreflexivity)
  /// - If `areInIncreasingOrder(a, b)` and `areInIncreasingOrder(b, c)` are
  ///   both `true`, then `areInIncreasingOrder(a, c)` is also `true`.
  ///   (Transitive comparability)
  /// - Two elements are *incomparable* if neither is ordered before the other
  ///   according to the predicate. If `a` and `b` are incomparable, and `b`
  ///   and `c` are incomparable, then `a` and `c` are also incomparable.
  ///   (Transitive incomparability)
  ///
  /// The sorting algorithm is not stable. A nonstable sort may change the
  /// relative order of elements for which `areInIncreasingOrder` does not
  /// establish an order.
  ///
  /// In the following example, the predicate provides an ordering for an array
  /// of a custom `HTTPResponse` type. The predicate orders errors before
  /// successes and sorts the error responses by their error code.
  ///
  ///     enum HTTPResponse {
  ///         case ok
  ///         case error(Int)
  ///     }
  ///
  ///     let responses: [HTTPResponse] = [.error(500), .ok, .ok, .error(404), .error(403)]
  ///     let sortedResponses = responses.sorted {
  ///         switch ($0, $1) {
  ///         // Order errors by code
  ///         case let (.error(aCode), .error(bCode)):
  ///             return aCode < bCode
  ///
  ///         // All successes are equivalent, so none is before any other
  ///         case (.ok, .ok): return false
  ///
  ///         // Order errors before successes
  ///         case (.error, .ok): return true
  ///         case (.ok, .error): return false
  ///         }
  ///     }
  ///     print(sortedResponses)
  ///     // Prints "[.error(403), .error(404), .error(500), .ok, .ok]"
  ///
  /// You also use this method to sort elements that conform to the
  /// `Comparable` protocol in descending order. To sort your sequence in
  /// descending order, pass the greater-than operator (`>`) as the
  /// `areInIncreasingOrder` parameter.
  ///
  ///     let students: Set = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
  ///     let descendingStudents = students.sorted(by: >)
  ///     print(descendingStudents)
  ///     // Prints "["Peter", "Kweku", "Kofi", "Akosua", "Abena"]"
  ///
  /// Calling the related `sorted()` method is equivalent to calling this
  /// method and passing the less-than operator (`<`) as the predicate.
  ///
  ///     print(students.sorted())
  ///     // Prints "["Abena", "Akosua", "Kofi", "Kweku", "Peter"]"
  ///     print(students.sorted(by: <))
  ///     // Prints "["Abena", "Akosua", "Kofi", "Kweku", "Peter"]"
  ///
  /// - Parameter areInIncreasingOrder: A predicate that returns `true` if its
  ///   first argument should be ordered before its second argument;
  ///   otherwise, `false`.
  /// - Returns: A sorted array of the sequence's elements.
  @inlinable
  public func sorted(
    by areInIncreasingOrder:
      (Element, Element) throws -> Bool
  ) rethrows -> [Element] {
    var result = ContiguousArray(self)
    try result.sort(by: areInIncreasingOrder)
    return Array(result)
  }
}

extension MutableCollection
  where
  Self : RandomAccessCollection, Element : Comparable {

  /// Sorts the collection in place.
  ///
  /// You can sort any mutable collection of elements that conform to the
  /// `Comparable` protocol by calling this method. Elements are sorted in
  /// ascending order.
  ///
  /// The sorting algorithm is not stable. A nonstable sort may change the
  /// relative order of elements that compare equal.
  ///
  /// Here's an example of sorting a list of students' names. Strings in Swift
  /// conform to the `Comparable` protocol, so the names are sorted in
  /// ascending order according to the less-than operator (`<`).
  ///
  ///     var students = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
  ///     students.sort()
  ///     print(students)
  ///     // Prints "["Abena", "Akosua", "Kofi", "Kweku", "Peter"]"
  ///
  /// To sort the elements of your collection in descending order, pass the
  /// greater-than operator (`>`) to the `sort(by:)` method.
  ///
  ///     students.sort(by: >)
  ///     print(students)
  ///     // Prints "["Peter", "Kweku", "Kofi", "Akosua", "Abena"]"
  @inlinable
  public mutating func sort() {
    let didSortUnsafeBuffer: Void? =
      _withUnsafeMutableBufferPointerIfSupported {
      (bufferPointer) -> Void in
      bufferPointer.sort()
      return ()
    }
    if didSortUnsafeBuffer == nil {
      _introSort(&self, subRange: startIndex..<endIndex)
    }
  }
}

extension MutableCollection where Self : RandomAccessCollection {
  /// Sorts the collection in place, using the given predicate as the
  /// comparison between elements.
  ///
  /// When you want to sort a collection of elements that doesn't conform to
  /// the `Comparable` protocol, pass a closure to this method that returns
  /// `true` when the first element passed should be ordered before the
  /// second.
  ///
  /// The predicate must be a *strict weak ordering* over the elements. That
  /// is, for any elements `a`, `b`, and `c`, the following conditions must
  /// hold:
  ///
  /// - `areInIncreasingOrder(a, a)` is always `false`. (Irreflexivity)
  /// - If `areInIncreasingOrder(a, b)` and `areInIncreasingOrder(b, c)` are
  ///   both `true`, then `areInIncreasingOrder(a, c)` is also `true`.
  ///   (Transitive comparability)
  /// - Two elements are *incomparable* if neither is ordered before the other
  ///   according to the predicate. If `a` and `b` are incomparable, and `b`
  ///   and `c` are incomparable, then `a` and `c` are also incomparable.
  ///   (Transitive incomparability)
  ///
  /// The sorting algorithm is not stable. A nonstable sort may change the
  /// relative order of elements for which `areInIncreasingOrder` does not
  /// establish an order.
  ///
  /// In the following example, the closure provides an ordering for an array
  /// of a custom enumeration that describes an HTTP response. The predicate
  /// orders errors before successes and sorts the error responses by their
  /// error code.
  ///
  ///     enum HTTPResponse {
  ///         case ok
  ///         case error(Int)
  ///     }
  ///
  ///     var responses: [HTTPResponse] = [.error(500), .ok, .ok, .error(404), .error(403)]
  ///     responses.sort {
  ///         switch ($0, $1) {
  ///         // Order errors by code
  ///         case let (.error(aCode), .error(bCode)):
  ///             return aCode < bCode
  ///
  ///         // All successes are equivalent, so none is before any other
  ///         case (.ok, .ok): return false
  ///
  ///         // Order errors before successes
  ///         case (.error, .ok): return true
  ///         case (.ok, .error): return false
  ///         }
  ///     }
  ///     print(responses)
  ///     // Prints "[.error(403), .error(404), .error(500), .ok, .ok]"
  ///
  /// Alternatively, use this method to sort a collection of elements that do
  /// conform to `Comparable` when you want the sort to be descending instead
  /// of ascending. Pass the greater-than operator (`>`) operator as the
  /// predicate.
  ///
  ///     var students = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
  ///     students.sort(by: >)
  ///     print(students)
  ///     // Prints "["Peter", "Kweku", "Kofi", "Akosua", "Abena"]"
  ///
  /// - Parameter areInIncreasingOrder: A predicate that returns `true` if its
  ///   first argument should be ordered before its second argument;
  ///   otherwise, `false`. If `areInIncreasingOrder` throws an error during
  ///   the sort, the elements may be in a different order, but none will be
  ///   lost.
  @inlinable
  public mutating func sort(
    by areInIncreasingOrder:
      (Element, Element) throws -> Bool
  ) rethrows {

    let didSortUnsafeBuffer: Void? =
      try _withUnsafeMutableBufferPointerIfSupported {
      (bufferPointer) -> Void in
      try bufferPointer.sort(by: areInIncreasingOrder)
      return ()
    }
    if didSortUnsafeBuffer == nil {
      try _introSort(
        &self,
        subRange: startIndex..<endIndex,
        by: areInIncreasingOrder)
    }
  }
}
