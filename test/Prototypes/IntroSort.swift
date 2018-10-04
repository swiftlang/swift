//===--- IntroSort.swift --------------------------------------*- swift -*-===//
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
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g -Onone -DUSE_STDLIBUNITTEST %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

#if USE_STDLIBUNITTEST
import Swift
import StdlibUnittest
#endif

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
    
    // FIXME: Stashing the pivot element instead of using the index won't work
    // for move-only types.
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
    var idx = idx
    var countToIndex = distance(from: range.lowerBound, to: idx)
    var countFromIndex = distance(from: idx, to: range.upperBound)
    // Check if left child is within bounds. If not, stop iterating, because
    // there are no children of the given node in the heap.
    while countToIndex + 1 < countFromIndex {
      let left = index(idx, offsetBy: countToIndex + 1)
      var largest = idx
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
      // If a child is bigger than the current node, swap them and continue
      // sifting down.
      if largest != idx {
        swapAt(idx, largest)
        idx = largest
        countToIndex = distance(from: range.lowerBound, to: idx)
        countFromIndex = distance(from: idx, to: range.upperBound)
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
  public // @testable
  mutating func _heapSort(
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

//===--- Tests ------------------------------------------------------------===//
//===----------------------------------------------------------------------===//

var suite = TestSuite("IntroSort")

// The routine is based on http://www.cs.dartmouth.edu/~doug/mdmspe.pdf
func makeQSortKiller(_ len: Int) -> [Int] {
  var candidate: Int = 0
  var keys = [Int: Int]()
  func Compare(_ x: Int, y : Int) -> Bool {
    if keys[x] == nil && keys[y] == nil {
      if (x == candidate) {
        keys[x] = keys.count
      } else {
        keys[y] = keys.count
      }
    }
    if keys[x] == nil {
      candidate = x
      return true
    }
    if keys[y] == nil {
      candidate = y
      return false
    }
    return keys[x]! > keys[y]!
  }
  
  var ary = [Int](repeating: 0, count: len)
  var ret = [Int](repeating: 0, count: len)
  for i in 0..<len { ary[i] = i }
  ary = ary.sorted(by: Compare)
  for i in 0..<len {
    ret[ary[i]] = i
  }
  return ret
}

suite.test("sorted/complexity") {
  var ary: [Int] = []
  
  // Check performance of sorting an array of repeating values.
  var comparisons_100 = 0
  ary = Array(repeating: 0, count: 100)
  ary._introSort(within: 0..<ary.count) { comparisons_100 += 1; return $0 < $1 }
  var comparisons_1000 = 0
  ary = Array(repeating: 0, count: 1000)
  ary._introSort(within: 0..<ary.count) { comparisons_1000 += 1; return $0 < $1 }
  expectTrue(comparisons_1000/comparisons_100 < 20)
  
  // Try to construct 'bad' case for quicksort, on which the algorithm
  // goes quadratic.
  comparisons_100 = 0
  ary = makeQSortKiller(100)
  ary._introSort(within: 0..<ary.count) { comparisons_100 += 1; return $0 < $1 }
  comparisons_1000 = 0
  ary = makeQSortKiller(1000)
  ary._introSort(within: 0..<ary.count) { comparisons_1000 += 1; return $0 < $1 }
  expectTrue(comparisons_1000/comparisons_100 < 20)
}

suite.test("sort3/simple")
  .forEach(in: [
    [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]
  ]) {
    var input = $0
    input._sort3(0, 1, 2, by: <)
    expectEqual([1, 2, 3], input)
}

func isSorted<T>(_ a: [T], by areInIncreasingOrder: (T, T) -> Bool) -> Bool {
  return !zip(a.dropFirst(), a).contains(where: areInIncreasingOrder)
}

suite.test("sort3/stable")
  .forEach(in: [
    [1, 1, 2], [1, 2, 1], [2, 1, 1], [1, 2, 2], [2, 1, 2], [2, 2, 1], [1, 1, 1]
  ]) {
    // decorate with offset, but sort by value
    var input = Array($0.enumerated())
    input._sort3(0, 1, 2) { $0.element < $1.element }
    // offsets should still be ordered for equal values
    expectTrue(isSorted(input) {
      if $0.element == $1.element {
        return $0.offset < $1.offset
      }
      return $0.element < $1.element
    })
}

suite.test("heapSort") {
  // Generates the next permutation of `num` as a binary integer, using long
  // arithmetics approach.
  //
  // - Precondition: All values in `num` are either 0 or 1.
  func addOne(to num: [Int]) -> [Int] {
    // `num` represents a binary integer. To add one, we toggle any bits until
    // we've set a clear bit.
    var num = num
    for i in num.indices {
      if num[i] == 1 {
        num[i] = 0
      } else {
        num[i] = 1
        break
      }
    }
    return num
  }
  
  // Test binary number size.
  let numberLength = 11
  var binaryNumber = Array(repeating: 0, count: numberLength)
  
  // We are testing sort on all permutations off 0-1s of size `numberLength`
  // except the all 1's case (its equals to all 0's case).
  while !binaryNumber.allSatisfy({ $0 == 1 }) {
    var buffer = binaryNumber
    buffer._heapSort(within: buffer.startIndex..<buffer.endIndex, by: <)
    expectTrue(isSorted(buffer, by: <))
    binaryNumber = addOne(to: binaryNumber)
  }
}

runAllTests()

