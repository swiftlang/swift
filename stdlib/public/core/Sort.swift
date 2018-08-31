//===----------------------------------------------------------------------===//
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
// sorted()/sort()
//===----------------------------------------------------------------------===//

extension Sequence where Element: Comparable {
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
  ///
  /// - Complexity: O(*n* log *n*), where *n* is the length of the sequence.
  @inlinable
  public func sorted() -> [Element] {
    return sorted(by: <)
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
  ///
  /// - Complexity: O(*n* log *n*), where *n* is the length of the sequence.
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
where Self: RandomAccessCollection, Element: Comparable {
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
  ///
  /// - Complexity: O(*n* log *n*), where *n* is the length of the collection.
  @inlinable
  public mutating func sort() {
    sort(by: <)
  }
}

extension MutableCollection where Self: RandomAccessCollection {
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
  ///
  /// - Complexity: O(*n* log *n*), where *n* is the length of the collection.
  @inlinable
  public mutating func sort(
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {
    let didSortUnsafeBuffer = try _withUnsafeMutableBufferPointerIfSupported {
      buffer -> Void? in
        try buffer.sort(by: areInIncreasingOrder)
    }
    if didSortUnsafeBuffer == nil {
      try _introSort(within: startIndex..<endIndex, by: areInIncreasingOrder)
    }
  }
}

extension MutableCollection where Self: BidirectionalCollection {
  @inlinable
  internal mutating func _insertionSort(
    within range: Range<Index>, 
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {

    guard !range.isEmpty else { return }

    let start = range.lowerBound

    // Keep track of the end of the initial sequence of sorted elements. One
    // element is trivially already-sorted, thus pre-increment Continue until
    // the sorted elements cover the whole sequence
    var sortedEnd = index(after: start)

    while sortedEnd != range.upperBound {
      // get the first unsorted element
      // FIXME: by stashing the element, instead of using indexing and swapAt,
      // this method won't work for collections of move-only types.
      let x = self[sortedEnd]

      // Look backwards for x's position in the sorted sequence,
      // moving elements forward to make room.
      var i = sortedEnd
      repeat {
        let j = index(before: i)
        let predecessor = self[j]

        // If closure throws, put the element at right place and rethrow.
        do {
          // if x doesn't belong before y, we've found its position
          if try !areInIncreasingOrder(x, predecessor) {
            break
          }
        } catch {
          self[i] = x
          throw error
        }

        // Move y forward
        self[i] = predecessor
        i = j
      } while i != start

      if i != sortedEnd {
        // Plop x into position
        self[i] = x
      }
      formIndex(after: &sortedEnd)
    }
  }
}

extension MutableCollection {
  /// Sorts the elements at `elements[a]`, `elements[b]`, and `elements[c]`.
  /// Stable.
  ///
  /// The indices passed as `a`, `b`, and `c` do not need to be consecutive, but
  /// must be in strict increasing order.
  ///
  /// - Precondition: `a < b && b < c`
  /// - Postcondition: `self[a] <= self[b] && self[b] <= self[c]`
  @inlinable
  public // @testable
  mutating func _sort3(
    _ a: Index, _ b: Index, _ c: Index, 
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {
    // There are thirteen possible permutations for the original ordering of
    // the elements at indices `a`, `b`, and `c`. The comments in the code below
    // show the relative ordering of the three elements using a three-digit
    // number as shorthand for the position and comparative relationship of
    // each element. For example, "312" indicates that the element at `a` is the
    // largest of the three, the element at `b` is the smallest, and the element
    // at `c` is the median. This hypothetical input array has a 312 ordering for
    // `a`, `b`, and `c`:
    //
    //      [ 7, 4, 3, 9, 2, 0, 3, 7, 6, 5 ]
    //        ^              ^           ^
    //        a              b           c
    //
    // - If each of the three elements is distinct, they could be ordered as any
    //   of the permutations of 1, 2, and 3: 123, 132, 213, 231, 312, or 321.
    // - If two elements are equivalent and one is distinct, they could be
    //   ordered as any permutation of 1, 1, and 2 or 1, 2, and 2: 112, 121, 211,
    //   122, 212, or 221.
    // - If all three elements are equivalent, they are already in order: 111.

    switch try (areInIncreasingOrder(self[b], self[a]),
                areInIncreasingOrder(self[c], self[b])) {
    case (false, false):
      // 0 swaps: 123, 112, 122, 111
      break

    case (true, true):
      // 1 swap: 321
      // swap(a, c): 312->123
      swapAt(a, c)

    case (true, false):
      // 1 swap: 213, 212 --- 2 swaps: 312, 211
      // swap(a, b): 213->123, 212->122, 312->132, 211->121
      swapAt(a, b)

      if try areInIncreasingOrder(self[c], self[b]) {
        // 132 (started as 312), 121 (started as 211)
        // swap(b, c): 132->123, 121->112
        swapAt(b, c)
      }

    case (false, true):
      // 1 swap: 132, 121 --- 2 swaps: 231, 221
      // swap(b, c): 132->123, 121->112, 231->213, 221->212
      swapAt(b, c)

      if try areInIncreasingOrder(self[b], self[a]) {
        // 213 (started as 231), 212 (started as 221)
        // swap(a, b): 213->123, 212->122
        swapAt(a, b)
      }
    }
  }
}

extension MutableCollection where Self: RandomAccessCollection {
  /// Reorders the collection and returns an index `p` such that every element
  /// in `range.lowerBound..<p` is less than every element in
  /// `p..<range.upperBound`.
  ///
  /// - Precondition: The count of `range` must be >= 3 i.e.
  ///   `distance(from: range.lowerBound, to: range.upperBound) >= 3`
  @inlinable
  internal mutating func _partition(
    within range: Range<Index>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows -> Index {
    var lo = range.lowerBound
    var hi = index(before: range.upperBound)

    // Sort the first, middle, and last elements, then use the middle value
    // as the pivot for the partition.
    let half = distance(from: lo, to: hi) / 2
    let mid = index(lo, offsetBy: half)
    try _sort3(lo, mid, hi, by: areInIncreasingOrder)
    let pivot = self[mid]

    // Loop invariants:
    // * lo < hi
    // * self[i] < pivot, for i in range.lowerBound..<lo
    // * pivot <= self[i] for i in hi..<range.upperBound
    Loop: while true {
      FindLo: do {
        formIndex(after: &lo)
        while lo != hi {
          if try !areInIncreasingOrder(self[lo], pivot) { break FindLo }
          formIndex(after: &lo)
        }
        break Loop
      }

      FindHi: do {
        formIndex(before: &hi)
        while hi != lo {
          if try areInIncreasingOrder(self[hi], pivot) { break FindHi }
          formIndex(before: &hi)
        }
        break Loop
      }

      swapAt(lo, hi)
    }

    return lo
  }

  @inlinable
  public // @testable
  mutating func _introSort(
    within range: Range<Index>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {

    let n = distance(from: range.lowerBound, to: range.upperBound)
    guard n > 1 else { return }

    // Set max recursion depth to 2*floor(log(N)), as suggested in the introsort
    // paper: http://www.cs.rpi.edu/~musser/gp/introsort.ps
    let depthLimit = 2 * n._binaryLogarithm()
    try _introSortImpl(
      within: range,
      by: areInIncreasingOrder,
      depthLimit: depthLimit)
  }

  @inlinable
  internal mutating func _introSortImpl(
    within range: Range<Index>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool,
    depthLimit: Int
  ) rethrows {

    // Insertion sort is better at handling smaller regions.
    if distance(from: range.lowerBound, to: range.upperBound) < 20 {
      try _insertionSort(within: range, by: areInIncreasingOrder)
    } else if depthLimit == 0 {
      try _heapSort(within: range, by: areInIncreasingOrder)
    } else {
      // Partition and sort.
      // We don't check the depthLimit variable for underflow because this
      // variable is always greater than zero (see check above).
      let partIdx = try _partition(within: range, by: areInIncreasingOrder)
      try _introSortImpl(
        within: range.lowerBound..<partIdx,
        by: areInIncreasingOrder, 
        depthLimit: depthLimit &- 1)
      try _introSortImpl(
        within: partIdx..<range.upperBound,
        by: areInIncreasingOrder, 
        depthLimit: depthLimit &- 1)      
    }
  }

  @inlinable
  internal mutating func _siftDown(
    _ idx: Index,
    within range: Range<Index>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {
    var i = idx
    var countToIndex = distance(from: range.lowerBound, to: i)
    var countFromIndex = distance(from: i, to: range.upperBound)
    // Check if left child is within bounds. If not, stop iterating, because
    // there are no children of the given node in the heap.
    while countToIndex + 1 < countFromIndex {
      let left = index(i, offsetBy: countToIndex + 1)
      var largest = i
      if try areInIncreasingOrder(self[largest], self[left]) {
        largest = left
      }
      // Check if right child is also within bounds before trying to examine it.
      if countToIndex + 2 < countFromIndex {
        let right = index(after: left)
        if try areInIncreasingOrder(self[largest], self[right]) {
          largest = right
        }
      }
      // If a child is bigger than the current node, swap them and continue sifting
      // down.
      if largest != i {
        swapAt(idx, largest)
        i = largest
        countToIndex = distance(from: range.lowerBound, to: i)
        countFromIndex = distance(from: i, to: range.upperBound)
      } else {
        break
      }
    }
  }

  @inlinable
  internal mutating func _heapify(
    within range: Range<Index>, 
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {
    // Here we build a heap starting from the lowest nodes and moving to the
    // root. On every step we sift down the current node to obey the max-heap
    // property:
    //   parent >= max(leftChild, rightChild)
    //
    // We skip the rightmost half of the array, because these nodes don't have
    // any children.
    let root = range.lowerBound
    let half = distance(from: range.lowerBound, to: range.upperBound) / 2
    var node = index(root, offsetBy: half)

    while node != root {
      formIndex(before: &node)
      try _siftDown(node, within: range, by: areInIncreasingOrder)
    }
  }

  @inlinable
  internal mutating func _heapSort(
    within range: Range<Index>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {
    var hi = range.upperBound
    let lo = range.lowerBound
    try _heapify(within: range, by: areInIncreasingOrder)
    formIndex(before: &hi)
    while hi != lo {
      swapAt(lo, hi)
      try _siftDown(lo, within: lo..<hi, by: areInIncreasingOrder)
      formIndex(before: &hi)
    }
  }
}
