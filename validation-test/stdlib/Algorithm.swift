// -*- swift -*-
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import SwiftPrivate

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
#if _runtime(_ObjC)
import ObjectiveC
#endif

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
  let b3 = MinimalComparableValue(1, identity: 6)
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

Algorithm.test("sorted/strings")
  .xfail(.LinuxAny(reason: "String comparison: ICU vs. Foundation"))
  .code {
  expectEqual(
    [ "Banana", "apple", "cherry" ],
    [ "apple", "Banana", "cherry" ].sort())

  let s = ["apple", "Banana", "cherry"].sort() {
    $0.characters.count > $1.characters.count
  }
  expectEqual([ "Banana", "cherry", "apple" ], s)
}

// A wrapper around Array<T> that disables any type-specific algorithm
// optimizations and forces bounds checking on.
struct A<T> : MutableSliceable {
  init(_ a: Array<T>) {
    impl = a
  }

  var startIndex: Int {
    return 0
  }

  var endIndex: Int {
    return impl.count
  }

  func generate() -> Array<T>.Generator {
    return impl.generate()
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
      expectTrue(r.startIndex >= 0 && r.startIndex <= impl.count)
      expectTrue(r.endIndex >= 0 && r.endIndex <= impl.count)
      return impl[r]
    }
    set (x) {
      expectTrue(r.startIndex >= 0 && r.startIndex <= impl.count)
      expectTrue(r.endIndex >= 0 && r.endIndex <= impl.count)
      impl[r] = x
    }
  }

  var impl: Array<T>
}

func randomArray() -> A<Int> {
  let count = Int(rand32(exclusiveUpperBound: 50))
  return A(randArray(count))
}

Algorithm.test("invalidOrderings") {
  withInvalidOrderings {
    var a = randomArray()
    _blackHole(a.sort($0))
  }
  withInvalidOrderings {
    var a: A<Int>
    a = randomArray()
    a.partition(a.indices, isOrderedBefore: $0)
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
func makeQSortKiller(len: Int) -> [Int] {
  var candidate: Int = 0
  var keys = [Int:Int]()
  func Compare(x: Int, y : Int) -> Bool {
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

  var ary = [Int](count: len, repeatedValue:0)
  var ret = [Int](count: len, repeatedValue:0)
  for i in 0..<len { ary[i] = i }
  ary = ary.sort(Compare)
  for i in 0..<len {
    ret[ary[i]] = i
  }
  return ret
}

Algorithm.test("sorted/complexity") {
  var ary: [Int] = []

  // Check performance of sort on array of repeating values
  var comparisons_100 = 0
  ary = [Int](count: 100, repeatedValue: 0)
  ary.sortInPlace { comparisons_100++; return $0 < $1 }
  var comparisons_1000 = 0
  ary = [Int](count: 1000, repeatedValue: 0)
  ary.sortInPlace { comparisons_1000++; return $0 < $1 }
  expectTrue(comparisons_1000/comparisons_100 < 20)

  // Try to construct 'bad' case for quicksort, on which the algorithm
  // goes quadratic.
  comparisons_100 = 0
  ary = makeQSortKiller(100)
  ary.sortInPlace { comparisons_100++; return $0 < $1 }
  comparisons_1000 = 0
  ary = makeQSortKiller(1000)
  ary.sortInPlace { comparisons_1000++; return $0 < $1 }
  expectTrue(comparisons_1000/comparisons_100 < 20)
}

Algorithm.test("sorted/return type") {
  let x: Array = ([5, 4, 3, 2, 1] as ArraySlice).sort()
}

runAllTests()

