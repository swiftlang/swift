// -*- swift -*-
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest
import SwiftPrivate


var Algorithm = TestSuite("Algorithm")

// FIXME(prext): remove this conformance.
extension String.UnicodeScalarView : Equatable {}

// FIXME(prext): remove this function.
public func == (
  lhs: String.UnicodeScalarView, rhs: String.UnicodeScalarView) -> Bool {
  return Array(lhs) == Array(rhs)
}

// FIXME(prext): move this struct to the point of use.
Algorithm.test("min,max") {
  // Identities are unique in this set.
  let a1 = MinimalComparableValue(0, identity: 1)
  let a2 = MinimalComparableValue(0, identity: 2)
  let a3 = MinimalComparableValue(0, identity: 3)
  let b1 = MinimalComparableValue(1, identity: 4)
  let b2 = MinimalComparableValue(1, identity: 5)
  _ = MinimalComparableValue(1, identity: 6)
  let c1 = MinimalComparableValue(2, identity: 7)
  let c2 = MinimalComparableValue(2, identity: 8)
  let c3 = MinimalComparableValue(2, identity: 9)

  // 2-arg min()
  expectEqual(a1.identity, min(a1, b1).identity)
  expectEqual(a1.identity, min(b1, a1).identity)
  expectEqual(a1.identity, min(a1, a2).identity)

  // 2-arg max()
  expectEqual(c1.identity, max(c1, b1).identity)
  expectEqual(c1.identity, max(b1, c1).identity)
  expectEqual(c1.identity, max(c2, c1).identity)

  // 3-arg min()
  expectEqual(a1.identity, min(a1, b1, c1).identity)
  expectEqual(a1.identity, min(b1, a1, c1).identity)
  expectEqual(a1.identity, min(c1, b1, a1).identity)
  expectEqual(a1.identity, min(c1, a1, b1).identity)
  expectEqual(a1.identity, min(a1, a2, a3).identity)
  expectEqual(a1.identity, min(a1, a2, b1).identity)
  expectEqual(a1.identity, min(a1, b1, a2).identity)
  expectEqual(a1.identity, min(b1, a1, a2).identity)
 
  // 3-arg max()
  expectEqual(c1.identity, max(c1, b1, a1).identity)
  expectEqual(c1.identity, max(a1, c1, b1).identity)
  expectEqual(c1.identity, max(b1, a1, c1).identity)
  expectEqual(c1.identity, max(b1, c1, a1).identity)
  expectEqual(c1.identity, max(c3, c2, c1).identity)
  expectEqual(c1.identity, max(c2, c1, b1).identity)
  expectEqual(c1.identity, max(c2, b1, c1).identity)
  expectEqual(c1.identity, max(b1, c2, c1).identity)
  
  // 4-arg min()
  expectEqual(a1.identity, min(a1, b1, a2, b2).identity)
  expectEqual(a1.identity, min(b1, a1, a2, b2).identity)
  expectEqual(a1.identity, min(c1, b1, b2, a1).identity)
  expectEqual(a1.identity, min(c1, b1, a1, a2).identity)
  
  // 4-arg max()
  expectEqual(c1.identity, max(c2, b1, c1, b2).identity)
  expectEqual(c1.identity, max(b1, c2, c1, b2).identity)
  expectEqual(c1.identity, max(a1, b1, b2, c1).identity)
  expectEqual(c1.identity, max(a1, b1, c2, c1).identity)
}

Algorithm.test("sorted/strings") {
  expectEqual(
    ["Banana", "apple", "cherry"],
    ["apple", "Banana", "cherry"].sorted())

  let s = ["apple", "Banana", "cherry"].sorted() {
    $0.count > $1.count
  }
  expectEqual(["Banana", "cherry", "apple"], s)
}

// A wrapper around Array<T> that disables any type-specific algorithm
// optimizations and forces bounds checking on.
struct A<T> : MutableCollection, RandomAccessCollection {
  typealias Indices = CountableRange<Int>

  init(_ a: Array<T>) {
    impl = a
  }

  var startIndex: Int {
    return 0
  }

  var endIndex: Int {
    return impl.count
  }

  func makeIterator() -> Array<T>.Iterator {
    return impl.makeIterator()
  }

  subscript(i: Int) -> T {
    get {
      expectTrue(i >= 0 && i < impl.count)
      return impl[i]
    }
    set (x) {
      expectTrue(i >= 0 && i < impl.count)
      impl[i] = x
    }
  }

  subscript(r: Range<Int>) -> Array<T>.SubSequence {
    get {
      expectTrue(r.lowerBound >= 0 && r.lowerBound <= impl.count)
      expectTrue(r.upperBound >= 0 && r.upperBound <= impl.count)
      return impl[r]
    }
    set (x) {
      expectTrue(r.lowerBound >= 0 && r.lowerBound <= impl.count)
      expectTrue(r.upperBound >= 0 && r.upperBound <= impl.count)
      impl[r] = x
    }
  }

  var impl: Array<T>
}

func randomArray() -> A<Int> {
  let count = Int.random(in: 0 ..< 50)
  let array = (0 ..< count).map { _ in Int.random(in: .min ... .max) }
  return A(array)
}

Algorithm.test("invalidOrderings") {
  withInvalidOrderings {
    let a = randomArray()
    _blackHole(a.sorted(by: $0))
  }
  withInvalidOrderings {
    var a: A<Int>
    a = randomArray()
    let lt = $0
    let first = a.first
    _ = a.partition(by: { !lt($0, first!) })
  }
  /*
  // FIXME: Disabled due to <rdar://problem/17734737> Unimplemented:
  // abstraction difference in l-value
  withInvalidOrderings {
    var a = randomArray()
    var pred = $0
    _insertionSort(&a, a.indices, &pred)
  }
  */
}

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

Algorithm.test("sorted/complexity") {
  var ary: [Int] = []

  // Check performance of sorting an array of repeating values.
  var comparisons_100 = 0
  ary = [Int](repeating: 0, count: 100)
  ary.sort { comparisons_100 += 1; return $0 < $1 }
  var comparisons_1000 = 0
  ary = [Int](repeating: 0, count: 1000)
  ary.sort { comparisons_1000 += 1; return $0 < $1 }
  expectTrue(comparisons_1000/comparisons_100 < 20)

  // Try to construct 'bad' case for quicksort, on which the algorithm
  // goes quadratic.
  comparisons_100 = 0
  ary = makeQSortKiller(100)
  ary.sort { comparisons_100 += 1; return $0 < $1 }
  comparisons_1000 = 0
  ary = makeQSortKiller(1000)
  ary.sort { comparisons_1000 += 1; return $0 < $1 }
  expectTrue(comparisons_1000/comparisons_100 < 20)
}

Algorithm.test("sorted/return type") {
  let _: Array = ([5, 4, 3, 2, 1] as ArraySlice).sorted()
}

Algorithm.test("sort3/simple")
  .forEach(in: [
    [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]
  ]) {
    var input = $0
    input._sort3(0, 1, 2, by: <)
    expectEqual([1, 2, 3], input)
}

func isSorted<T>(_ a: [T], by areInIncreasingOrder: (T, T) -> Bool) -> Bool {
  return !a.dropFirst().enumerated().contains(where: { (offset, element) in
    areInIncreasingOrder(element, a[offset])
  })
}

Algorithm.test("sort3/stable")
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

Algorithm.test("heapSort") {
  // This function generate next permutation of 0-1 using long arithmetics 
  // approach.
  func addOne(to num: inout [Int]) {

    if num.isEmpty {
      return
    }
    // Consider our num array reflects a binary integer.
    // Here we are trying to add one to it.
    var i = num.index(before: num.endIndex)
    var carrier = 1

    while carrier != 0 {
      let b = num[i] + carrier
      // Updating i's bit.
      num[i] = b % 2
      // Recalculate new carrier.
      carrier = b / 2

      // If the length of number was n, we don't want to create new number with
      // length n + 1 in this test.
      if i == num.startIndex {
        break
      } else {
        num.formIndex(before: &i)
      }
    }
  } // addOne end.

  // Test binary number size.
  let numberLength = 11
  var binaryNumber = [Int](repeating: 0, count: numberLength)

  // We are testing sort on all permutations off 0-1s of size `numberLength`
  // except the all 1's case (Its equals to all 0's case).
  while !binaryNumber.allSatisfy({ $0 == 1 }) {
    var buffer = binaryNumber

    buffer._heapSort(within: buffer.startIndex..<buffer.endIndex, by: <)

    expectTrue(isSorted(buffer, by: <))

    addOne(to: &binaryNumber)
  }
}

runAllTests()

