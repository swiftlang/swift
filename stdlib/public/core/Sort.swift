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
  /// The sorting algorithm is not guaranteed to be stable. A stable sort
  /// preserves the relative order of elements that compare equal.
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
  /// `true` when the first element should be ordered before the second. The
  /// elements of the resulting array are ordered according to the given
  /// predicate.
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
  /// The sorting algorithm is not guaranteed to be stable. A stable sort
  /// preserves the relative order of elements for which
  /// `areInIncreasingOrder` does not establish an order.
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
  /// The sorting algorithm is not guaranteed to be stable. A stable sort
  /// preserves the relative order of elements that compare equal.
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
  /// When you want to sort a collection of elements that don't conform to
  /// the `Comparable` protocol, pass a closure to this method that returns
  /// `true` when the first element should be ordered before the second.
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
  /// `areInIncreasingOrder` must be a *strict weak ordering* over the
  /// elements. That is, for any elements `a`, `b`, and `c`, the following
  /// conditions must hold:
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
  /// The sorting algorithm is not guaranteed to be stable. A stable sort
  /// preserves the relative order of elements for which
  /// `areInIncreasingOrder` does not establish an order.
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
        try buffer._stableSortImpl(by: areInIncreasingOrder)
    }
    if didSortUnsafeBuffer == nil {
      // Fallback since we can't use an unsafe buffer: sort into an outside
      // array, then copy elements back in.
      let sortedElements = try sorted(by: areInIncreasingOrder)
      for (i, j) in zip(indices, sortedElements.indices) {
        self[i] = sortedElements[j]
      }
    }
  }
}

extension MutableCollection where Self: BidirectionalCollection {
  /// Sorts `self[range]` according to `areInIncreasingOrder`. Stable.
  ///
  /// - Precondition: `sortedEnd != range.lowerBound`
  /// - Precondition: `elements[..<sortedEnd]` are already in order.
  @inlinable
  internal mutating func _insertionSort(
    within range: Range<Index>,
    sortedEnd: Index,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {
    var sortedEnd = sortedEnd
    
    // Continue sorting until the sorted elements cover the whole sequence.
    while sortedEnd != range.upperBound {
      var i = sortedEnd
      // Look backwards for `self[i]`'s position in the sorted sequence,
      // moving each element forward to make room.
      repeat {
        let j = index(before: i)
        
        // If `self[i]` doesn't belong before `self[j]`, we've found
        // its position.
        if try !areInIncreasingOrder(self[i], self[j]) {
          break
        }
        
        swapAt(i, j)
        i = j
      } while i != range.lowerBound
      
      formIndex(after: &sortedEnd)
    }
  }
  
  /// Sorts `self[range]` according to `areInIncreasingOrder`. Stable.
  @inlinable
  public // @testable
  mutating func _insertionSort(
    within range: Range<Index>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {
    if range.isEmpty {
      return
    }
    
    // One element is trivially already-sorted, so the actual sort can
    // start on the second element.
    let sortedEnd = index(after: range.lowerBound)
    try _insertionSort(
      within: range, sortedEnd: sortedEnd, by: areInIncreasingOrder)
  }
  
  /// Reverses the elements in the given range.
  @inlinable
  internal mutating func _reverse(
    within range: Range<Index>
  ) {
    var f = range.lowerBound
    var l = range.upperBound
    while f < l {
      formIndex(before: &l)
      swapAt(f, l)
      formIndex(after: &f)
    }
  }
}

/// Merges the elements in the ranges `lo..<mid` and `mid..<hi` using `buffer`
/// as out-of-place storage. Stable.
///
/// - Precondition: `lo..<mid` and `mid..<hi` must already be sorted according
///   to `areInIncreasingOrder`.
/// - Precondition: `buffer` must point to a region of memory at least as large
///   as `min(mid - lo, hi - mid)`.
/// - Postcondition: `lo..<hi` is sorted according to `areInIncreasingOrder`.
@inlinable
internal func _merge<Element>(
  low: UnsafeMutablePointer<Element>,
  mid: UnsafeMutablePointer<Element>,
  high: UnsafeMutablePointer<Element>,
  buffer: UnsafeMutablePointer<Element>,
  by areInIncreasingOrder: (Element, Element) throws -> Bool
) rethrows -> Bool {
  let lowCount = mid - low
  let highCount = high - mid
  
  var destLow = low         // Lower bound of uninitialized storage
  var bufferLow = buffer    // Lower bound of the initialized buffer
  var bufferHigh = buffer   // Upper bound of the initialized buffer

  // When we exit the merge, move any remaining elements from the buffer back
  // into `destLow` (aka the collection we're sorting). The buffer can have
  // remaining elements if `areIncreasingOrder` throws, or more likely if the
  // merge runs out of elements from the array before exhausting the buffer.
  defer {
    destLow.moveInitialize(from: bufferLow, count: bufferHigh - bufferLow)
  }
  
  if lowCount < highCount {
    // Move the lower group of elements into the buffer, then traverse from
    // low to high in both the buffer and the higher group of elements.
    //
    // After moving elements, the storage and buffer look like this, where
    // `x` is uninitialized memory:
    //
    // Storage: [x, x, x, x, x, 6, 8, 8, 10, 12, 15]
    //           ^              ^
    //        destLow        srcLow
    //
    // Buffer:  [4, 4, 7, 8, 9, x, ...]
    //           ^              ^
    //        bufferLow     bufferHigh
    buffer.moveInitialize(from: low, count: lowCount)
    bufferHigh = bufferLow + lowCount
    
    var srcLow = mid

    // Each iteration moves the element that compares lower into `destLow`,
    // preferring the buffer when equal to maintain stability. Elements are
    // moved from either `bufferLow` or `srcLow`, with those pointers
    // incrementing as elements are moved.
    while bufferLow < bufferHigh && srcLow < high {
      if try areInIncreasingOrder(srcLow.pointee, bufferLow.pointee) {
        destLow.moveInitialize(from: srcLow, count: 1)
        srcLow += 1
      } else {
        destLow.moveInitialize(from: bufferLow, count: 1)
        bufferLow += 1
      }
      destLow += 1
    }
  } else {
    // Move the higher group of elements into the buffer, then traverse from
    // high to low in both the buffer and the lower group of elements.
    //
    // After moving elements, the storage and buffer look like this, where
    // `x` is uninitialized memory:
    //
    // Storage: [4, 4, 7, 8, 9, 6, x, x,  x,  x,  x]
    //                          ^  ^                 ^
    //                    srcHigh  destLow        destHigh (past the end)
    //
    // Buffer:                    [8, 8, 10, 12, 15, x, ...]
    //                             ^                 ^
    //                          bufferLow        bufferHigh
    buffer.moveInitialize(from: mid, count: highCount)
    bufferHigh = bufferLow + highCount
    
    var destHigh = high
    var srcHigh = mid
    destLow = mid

    // Each iteration moves the element that compares higher into `destHigh`,
    // preferring the buffer when equal to maintain stability. Elements are
    // moved from either `bufferHigh - 1` or `srcHigh - 1`, with those
    // pointers decrementing as elements are moved.
    //
    // Note: At the start of each iteration, each `...High` pointer points one
    // past the element they're referring to.
    while bufferHigh > bufferLow && srcHigh > low {
      destHigh -= 1
      if try areInIncreasingOrder(
        (bufferHigh - 1).pointee, (srcHigh - 1).pointee
      ) {
        srcHigh -= 1
        destHigh.moveInitialize(from: srcHigh, count: 1)
        
        // Moved an element from the lower initialized portion to the upper,
        // sorted, initialized portion, so `destLow` moves down one.
        destLow -= 1
      } else {
        bufferHigh -= 1
        destHigh.moveInitialize(from: bufferHigh, count: 1)
      }
    }
  }

  // FIXME: Remove this, it works around rdar://problem/45044610
  return true
}

/// Calculates an optimal minimum run length for sorting a collection.
///
/// "... pick a minrun in range(32, 65) such that N/minrun is exactly a power
/// of 2, or if that isn't possible, is close to, but strictly less than, a
/// power of 2. This is easier to do than it may sound: take the first 6 bits
/// of N, and add 1 if any of the remaining bits are set."
/// - From the Timsort introduction, at
///   https://svn.python.org/projects/python/trunk/Objects/listsort.txt
///
/// - Parameter c: The number of elements in a collection.
/// - Returns: If `c <= 64`, returns `c`. Otherwise, returns a value in
///   `32...64`.
@inlinable
internal func _minimumMergeRunLength(_ c: Int) -> Int {
  // Max out at `2^6 == 64` elements
  let bitsToUse = 6
  
  if c < 1 << bitsToUse {
    return c
  }
  let offset = (Int.bitWidth - bitsToUse) - c.leadingZeroBitCount
  let mask = (1 << offset) - 1
  return c >> offset + (c & mask == 0 ? 0 : 1)
}

/// Returns the end of the next in-order run along with a Boolean value
/// indicating whether the elements in `start..<end` are in descending order.
///
/// - Precondition: `start < elements.endIndex`
@inlinable
internal func _findNextRun<C: RandomAccessCollection>(
  in elements: C,
  from start: C.Index,
  by areInIncreasingOrder: (C.Element, C.Element) throws -> Bool
) rethrows -> (end: C.Index, descending: Bool) {
  _internalInvariant(start < elements.endIndex)

  var previous = start
  var current = elements.index(after: start)
  guard current < elements.endIndex else {
    // This is a one-element run, so treating it as ascending saves a
    // meaningless call to `reverse()`.
    return (current, false)
  }

  // Check whether the run beginning at `start` is ascending or descending.
  // An ascending run can include consecutive equal elements, but because a
  // descending run will be reversed, it must be strictly descending.
  let isDescending =
    try areInIncreasingOrder(elements[current], elements[previous])
  
  // Advance `current` until there's a break in the ascending / descending
  // pattern.
  repeat {
    previous = current
    elements.formIndex(after: &current)
  } while try current < elements.endIndex &&
    isDescending == areInIncreasingOrder(elements[current], elements[previous])
    
  return(current, isDescending)
}

extension UnsafeMutableBufferPointer {
  /// Merges the elements at `runs[i]` and `runs[i - 1]`, using `buffer` as
  /// out-of-place storage.
  ///
  /// - Precondition: `runs.count > 1` and `i > 0`
  /// - Precondition: `buffer` must have at least
  ///   `min(runs[i].count, runs[i - 1].count)` uninitialized elements.
  @inlinable
  public mutating func _mergeRuns(
    _ runs: inout [Range<Index>],
    at i: Int,
    buffer: UnsafeMutablePointer<Element>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows -> Bool {
    _internalInvariant(runs[i - 1].upperBound == runs[i].lowerBound)
    let low = runs[i - 1].lowerBound
    let middle = runs[i].lowerBound
    let high = runs[i].upperBound
    
    let result = try _merge(
      low: baseAddress! + low,
      mid: baseAddress! + middle,
      high: baseAddress! + high,
      buffer: buffer,
      by: areInIncreasingOrder)
    
    runs[i - 1] = low..<high
    runs.remove(at: i)

    // FIXME: Remove this, it works around rdar://problem/45044610
    return result
  }
  
  /// Merges upper elements of `runs` until the required invariants are
  /// satisfied.
  ///
  /// - Precondition: `buffer` must have at least
  ///   `min(runs[i].count, runs[i - 1].count)` uninitialized elements.
  /// - Precondition: The ranges in `runs` must be consecutive, such that for
  ///   any i, `runs[i].upperBound == runs[i + 1].lowerBound`.
  @inlinable
  public mutating func _mergeTopRuns(
    _ runs: inout [Range<Index>],
    buffer: UnsafeMutablePointer<Element>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows -> Bool {
    // The invariants for the `runs` array are:
    // (a) - for all i in 2..<runs.count:
    //         - runs[i - 2].count > runs[i - 1].count + runs[i].count
    // (b) - for c = runs.count - 1:
    //         - runs[i - 1].count > runs[i].count
    //
    // Loop until the invariant is satisified for the top four elements of
    // `runs`. Because this method is called for every added run, and only
    // the top three runs are ever merged, this guarantees the invariant holds
    // for the whole array.
    //
    // At all times, `runs` is one of the following, where W, X, Y, and Z are
    // the counts of their respective ranges:
    // - [ ...?, W, X, Y, Z ]
    // - [ X, Y, Z ]
    // - [ Y, Z ]
    //
    // If W > X + Y, X > Y + Z, and Y > Z, then the invariants are satisfied
    // for the entirety of `runs`.
    
    // FIXME: Remove this, it works around rdar://problem/45044610
    var result = true

    // The invariant is always in place for a single element.
    while runs.count > 1 {
      var lastIndex = runs.count - 1
      
      // Check for the three invariant-breaking conditions, and break out of
      // the while loop if none are met.
      if lastIndex >= 3 &&
        (runs[lastIndex - 3].count <=
          runs[lastIndex - 2].count + runs[lastIndex - 1].count)
      {
        // Second-to-last three runs do not follow W > X + Y.
        // Always merge Y with the smaller of X or Z.
        if runs[lastIndex - 2].count < runs[lastIndex].count {
          lastIndex -= 1
        }
      } else if lastIndex >= 2 &&
        (runs[lastIndex - 2].count <=
          runs[lastIndex - 1].count + runs[lastIndex].count)
      {
        // Last three runs do not follow X > Y + Z.
        // Always merge Y with the smaller of X or Z.
        if runs[lastIndex - 2].count < runs[lastIndex].count {
          lastIndex -= 1
        }
      } else if runs[lastIndex - 1].count <= runs[lastIndex].count {
        // Last two runs do not follow Y > Z, so merge Y and Z.
        // This block is intentionally blank--the merge happens below.
      } else {
        // All invariants satisfied!
        break
      }
      
      // Merge the runs at `i` and `i - 1`.
      result = try result && _mergeRuns(
        &runs, at: lastIndex, buffer: buffer, by: areInIncreasingOrder)
    }

    return result
  }
  
  /// Merges elements of `runs` until only one run remains.
  ///
  /// - Precondition: `buffer` must have at least
  ///   `min(runs[i].count, runs[i - 1].count)` uninitialized elements.
  /// - Precondition: The ranges in `runs` must be consecutive, such that for
  ///   any i, `runs[i].upperBound == runs[i + 1].lowerBound`.
  @inlinable
  public mutating func _finalizeRuns(
    _ runs: inout [Range<Index>],
    buffer: UnsafeMutablePointer<Element>,
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows -> Bool {
    // FIXME: Remove this, it works around rdar://problem/45044610
    var result = true
    while runs.count > 1 {
      result = try result && _mergeRuns(
        &runs, at: runs.count - 1, buffer: buffer, by: areInIncreasingOrder)
    }
    return result
  }
  
  /// Sorts the elements of this buffer according to `areInIncreasingOrder`,
  /// using a stable, adaptive merge sort.
  ///
  /// The adaptive algorithm used is Timsort, modified to perform a straight
  /// merge of the elements using a temporary buffer.
  @inlinable
  public mutating func _stableSortImpl(
    by areInIncreasingOrder: (Element, Element) throws -> Bool
  ) rethrows {
    let minimumRunLength = _minimumMergeRunLength(count)
    if count <= minimumRunLength {
      try _insertionSort(
        within: startIndex..<endIndex, by: areInIncreasingOrder)
      return
    }

    // FIXME: Remove this, it works around rdar://problem/45044610
    var result = true

    // Use array's allocating initializer to create a temporary buffer---this
    // keeps the buffer allocation going through the same tail-allocated path
    // as other allocating methods.
    //
    // There's no need to set the initialized count within the initializing
    // closure, since the buffer is guaranteed to be uninitialized at exit.
    _ = try Array<Element>(_unsafeUninitializedCapacity: count / 2) {
      buffer, _ in
      var runs: [Range<Index>] = []
      
      var start = startIndex
      while start < endIndex {
        // Find the next consecutive run, reversing it if necessary.
        var (end, descending) =
          try _findNextRun(in: self, from: start, by: areInIncreasingOrder)
        if descending {
          _reverse(within: start..<end)
        }
        
        // If the current run is shorter than the minimum length, use the
        // insertion sort to extend it.
        if end < endIndex && end - start < minimumRunLength {
          let newEnd = Swift.min(endIndex, start + minimumRunLength)
          try _insertionSort(
            within: start..<newEnd, sortedEnd: end, by: areInIncreasingOrder)
          end = newEnd
        }
        
        // Append this run and merge down as needed to maintain the `runs`
        // invariants.
        runs.append(start..<end)
        result = try result && _mergeTopRuns(
          &runs, buffer: buffer.baseAddress!, by: areInIncreasingOrder)
        start = end
      }
      
      result = try result && _finalizeRuns(
        &runs, buffer: buffer.baseAddress!, by: areInIncreasingOrder)
      assert(runs.count == 1, "Didn't complete final merge")
    }

    // FIXME: Remove this, it works around rdar://problem/45044610
    precondition(result)
  }
}
