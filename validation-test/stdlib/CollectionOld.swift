// RUN: %target-run-simple-swift --stdlib-unittest-in-process | tee %t.txt
// RUN: %FileCheck %s < %t.txt
// note: remove the --stdlib-unittest-in-process once all the FileCheck tests
// have been converted to StdlibUnittest
// REQUIRES: executable_test

// XFAIL: rdar45749460

import StdlibUnittest
import StdlibCollectionUnittest


var CollectionTests = TestSuite("CollectionTests")

/// An *iterator* that adapts a *collection* `C` and any *sequence* of
/// its `Index` type to present the collection's elements in a
/// permuted order.
public struct PermutationGenerator<
  C: Collection, Indices: Sequence
> : IteratorProtocol, Sequence where Indices.Element == C.Index {
  var seq : C
  var indices : Indices.Iterator

  /// The type of element returned by `next()`.
  public typealias Element = C.Element

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Precondition: No preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Element? {
    let result = indices.next()
    return result != nil ? seq[result!] : .none
  }

  /// Construct an *iterator* over a permutation of `elements` given
  /// by `indices`.
  ///
  /// - Precondition: `elements[i]` is valid for every `i` in `indices`.
  public init(elements: C, indices: Indices) {
    self.seq = elements
    self.indices = indices.makeIterator()
  }
}

var foobar = MinimalCollection(elements: "foobar")

// CHECK: foobar
for a in foobar {
  print(a, terminator: "")
}
print("")

// FIXME: separate r from the expression below pending
// <rdar://problem/15772601> Type checking failure
// CHECK: raboof
let i = foobar.indices
let r = i.lazy.reversed()
for a in PermutationGenerator(elements: foobar, indices: r) {
  
  print(a, terminator: "")
}
print("")

func isPalindrome0<S: BidirectionalCollection>(_ seq: S) -> Bool
where S.Element : Equatable {
  typealias Index = S.Index

  let a = seq.indices
  let i = seq.indices
  let ir = i.lazy.reversed()
  var b = ir.makeIterator()
  for i in a {
    if seq[i] != seq[b.next()!] {
      return false
    }
  }
  return true
}

// CHECK: false
print(isPalindrome0(MinimalBidirectionalCollection(elements: "GoHangaSalamiImaLasagneHoG")))
// CHECK: true
print(isPalindrome0(MinimalBidirectionalCollection(elements: "GoHangaSalamiimalaSagnaHoG")))

func isPalindrome1<
  S : BidirectionalCollection
>(_ seq: S) -> Bool
where S.Element : Equatable {
  let a = PermutationGenerator(elements: seq, indices: seq.indices)
  var b = seq.lazy.reversed().makeIterator()
  for nextChar in a {
    if nextChar != b.next()! {
      return false
    }
  }
  return true
}

func isPalindrome1_5<S: BidirectionalCollection>(_ seq: S) -> Bool
where S.Element: Equatable {
  var b = seq.lazy.reversed().makeIterator()
  for nextChar in seq {
    if nextChar != b.next()! {
      return false
    }
  }
  return true
}

// CHECK: false
print(isPalindrome1(MinimalBidirectionalCollection(elements: "MADAMINEDENIMWILLIAM")))
// CHECK: true
print(isPalindrome1(MinimalBidirectionalCollection(elements: "MadamInEdEnImadaM")))

// CHECK: false
print(isPalindrome1_5(MinimalBidirectionalCollection(elements: "FleetoMeRemoteelF")))
// CHECK: true
print(isPalindrome1_5(MinimalBidirectionalCollection(elements: "FleetoMeReMoteelF")))

// Finally, one that actually uses indexing to do half as much work.
// BidirectionalCollection traversal finally pays off!
func isPalindrome2<
  S: BidirectionalCollection
>(_ seq: S) -> Bool
where
S.Element: Equatable {

  var b = seq.startIndex, e = seq.endIndex

  while (b != e) {
    e = seq.index(before: e)
    if (b == e) {
      break
    }
    if seq[b] != seq[e] {
      return false
    }
    b = seq.index(after: b)
  }
  return true
}

// Test even length
// CHECK: false
print(isPalindrome2(MinimalBidirectionalCollection(elements: "ZerimarRamireZ")))
// CHECK: true
print(isPalindrome2(MinimalBidirectionalCollection(elements: "ZerimaRRamireZ")))

// Test odd length
// CHECK: false
print(isPalindrome2(MinimalBidirectionalCollection(elements: "ZerimarORamireZ")))
// CHECK: true
print(isPalindrome2(MinimalBidirectionalCollection(elements: "Zerimar-O-ramireZ")))

func isPalindrome4<
  S: BidirectionalCollection
>(_ seq: S) -> Bool
where
S.Element : Equatable {
  typealias Index = S.Index

  let a = PermutationGenerator(elements: seq, indices: seq.indices)
  // FIXME: separate ri from the expression below pending
  // <rdar://problem/15772601> Type checking failure
  let i = seq.indices
  let ri = i.lazy.reversed()
  var b = PermutationGenerator(elements: seq, indices: ri)
  for nextChar in a {
    if nextChar != b.next()! {
      return false
    }
  }
  return true
}

// Can't put these literals into string interpolations pending
// <rdar://problem/16401145> hella-slow compilation
let array = [1, 2, 3, 4]
let dict = [0:0, 1:1, 2:2, 3:3, 4:4]

func testCount() {
  // CHECK: testing count
  print("testing count")
  // CHECK-NEXT: random access: 4
  print("random access: \(array.count)")
  // CHECK-NEXT: bidirectional: 5
  print("bidirectional: \(dict.count)")
}
testCount()

struct SequenceOnly<T : Sequence> : Sequence {
  var base: T
  func makeIterator() -> T.Iterator { return base.makeIterator() }
}

func testUnderestimatedCount() {
  // CHECK: testing underestimatedCount
  print("testing underestimatedCount")
  // CHECK-NEXT: random access: 4
  print("random access: \(array.underestimatedCount)")
  // CHECK-NEXT: bidirectional: 5
  print("bidirectional: \(dict.underestimatedCount)")
  // CHECK-NEXT: Sequence only: 0
  let s = SequenceOnly(base: array)
  print("Sequence only: \(s.underestimatedCount)")
}
testUnderestimatedCount()

CollectionTests.test("isEmptyFirstLast") {
  expectTrue((10..<10).isEmpty)
  expectFalse((10...10).isEmpty)
  
  expectEqual(10, (10..<100).first)
  expectEqual(10, (10...100).first)
  expectEqual(99, (10..<100).last)
  expectEqual(100, (10...100).last)
}

/// A `Collection` that vends just the default implementations for
/// `CollectionType` methods.
struct CollectionOnly<T: Collection> : Collection {
  var base: T

  var startIndex: T.Index {
    return base.startIndex
  }

  var endIndex: T.Index {
    return base.endIndex
  }

  func makeIterator() -> T.Iterator {
    return base.makeIterator()
  }

  subscript(position: T.Index) -> T.Element {
    return base[position]
  }
  func index(after i: T.Index) -> T.Index { return base.index(after: i) }
}

// CHECK: all done.
print("all done.")

CollectionTests.test("first/performance") {
  // accessing `first` should not perform duplicate work on lazy collections
  var log: [Int] = []
  let col_ = (0..<10).lazy.filter({ log.append($0); return (2..<8).contains($0) })
  let col = CollectionOnly(base: col_)
  expectEqual(2, col.first)
  expectEqual([0, 1, 2], log)
}

runAllTests()
