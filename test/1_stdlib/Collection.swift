// RUN: %target-run-simple-swift --stdlib-unittest-in-process | tee %t.txt
// RUN: FileCheck %s < %t.txt
// note: remove the --stdlib-unittest-in-process once all the FileCheck tests
// have been converted to StdlibUnittest
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
#if _runtime(_ObjC)
import ObjectiveC
#endif

var CollectionTests = TestSuite("CollectionTests")

/// An *iterator* that adapts a *collection* `C` and any *sequence* of
/// its `Index` type to present the collection's elements in a
/// permuted order.
public struct PermutationGenerator<
  C: Collection, Indices: Sequence
  where
  C.Index == Indices.Iterator.Element
> : IteratorProtocol, Sequence {
  var seq : C
  var indices : Indices.Iterator

  /// The type of element returned by `next()`.
  public typealias Element = C.Iterator.Element

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: No preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Element? {
    let result = indices.next()
    return result != nil ? seq[result!] : .None
  }

  /// Construct an *iterator* over a permutation of `elements` given
  /// by `indices`.
  ///
  /// - Requires: `elements[i]` is valid for every `i` in `indices`.
  public init(elements: C, indices: Indices) {
    self.seq = elements
    self.indices = indices.iterator()
  }
}

struct X : Collection {
  typealias Element = String.CharacterView.Iterator.Element
  typealias Index = String.Index
  var msg: String

  init(_ msg: String) { self.msg = msg }
  var startIndex: Index {
    return msg.startIndex
  }
  var endIndex: Index {
    return msg.endIndex
  }
  subscript(i: Index) -> Element { return msg[i] }

  func iterator() -> IndexingIterator<X> {
    return IndexingIterator(self)
  }
}

var foobar = X("foobar")

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

func isPalindrome0<
  S : Collection
  where
  S.Index : BidirectionalIndex,
  S.Iterator.Element : Equatable
>(seq: S) -> Bool {
  typealias Index = S.Index

  let a = seq.indices
  var i = seq.indices
  var ir = i.lazy.reversed()
  var b = ir.iterator()
  for i in a {
    if seq[i] != seq[b.next()!] {
      return false
    }
  }
  return true
}

// CHECK: false
print(isPalindrome0(X("GoHangaSalamiImaLasagneHoG")))
// CHECK: true
print(isPalindrome0(X("GoHangaSalamiimalaSagnaHoG")))

func isPalindrome1<
  S : Collection
  where
  S.Index : BidirectionalIndex,
  S.Iterator.Element : Equatable
>(seq: S) -> Bool {

  var a = PermutationGenerator(elements: seq, indices: seq.indices)
  var b = seq.lazy.reversed().iterator()
  for nextChar in a {
    if nextChar != b.next()! {
      return false
    }
  }
  return true
}

func isPalindrome1_5<
  S: Collection
  where
  S.Index: BidirectionalIndex,
  S.Iterator.Element == S.Iterator.Element,
  S.Iterator.Element: Equatable
>(seq: S) -> Bool {

  var b = seq.lazy.reversed().iterator()
  for nextChar in seq {
    if nextChar != b.next()! {
      return false
    }
  }
  return true
}

// CHECK: false
print(isPalindrome1(X("MADAMINEDENIMWILLIAM")))
// CHECK: true
print(isPalindrome1(X("MadamInEdEnImadaM")))

// CHECK: false
print(isPalindrome1_5(X("FleetoMeRemoteelF")))
// CHECK: true
print(isPalindrome1_5(X("FleetoMeReMoteelF")))

// Finally, one that actually uses indexing to do half as much work.
// BidirectionalIndex traversal finally pays off!
func isPalindrome2<
  S: Collection
  where
  S.Index : BidirectionalIndex,
  S.Iterator.Element: Equatable
>(seq: S) -> Bool {

  var b = seq.startIndex, e = seq.endIndex

  while (b != e) {
    e = e.predecessor()
    if (b == e) {
      break
    }
    if seq[b] != seq[e] {
      return false
    }
    b = b.successor()
  }
  return true
}

// Test even length
// CHECK: false
print(isPalindrome2(X("ZerimarRamireZ")))
// CHECK: true
print(isPalindrome2(X("ZerimaRRamireZ")))

// Test odd length
// CHECK: false
print(isPalindrome2(X("ZerimarORamireZ")))
// CHECK: true
print(isPalindrome2(X("Zerimar-O-ramireZ")))

func isPalindrome4<
  S: Collection
  where
  S.Index : BidirectionalIndex,
  S.Iterator.Element : Equatable
>(seq: S) -> Bool {
  typealias Index = S.Index

  var a = PermutationGenerator(elements: seq, indices: seq.indices)
  // FIXME: separate ri from the expression below pending
  // <rdar://problem/15772601> Type checking failure
  var i = seq.indices
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
  func iterator() -> T.Iterator { return base.iterator() }
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

func testIsEmptyFirstLast() {
  // CHECK: testing isEmpty
  print("testing isEmpty")
  // CHECK-NEXT: true
  print((10..<10).isEmpty)
  // CHECK-NEXT: false
  print((10...10).isEmpty)
  // CHECK-NEXT: 10
  print((10..<100).first)
  // CHECK-NEXT: 99
  print((10..<100).last)
}
testIsEmptyFirstLast()

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

  func iterator() -> T.Iterator {
    return base.iterator()
  }

  subscript(position: T.Index) -> T.Iterator.Element {
    return base[position]
  }
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
