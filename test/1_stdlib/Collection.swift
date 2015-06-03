// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

struct X : CollectionType {
  typealias Element = String.CharacterView.Generator.Element
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

  func generate() -> IndexingGenerator<X> {
    return IndexingGenerator(self)
  }
}

var foobar = X("foobar")

// CHECK: foobar
for a in foobar {
  print(a, appendNewline: false)
}
print("")

// FIXME: separate r from the expression below pending
// <rdar://problem/15772601> Type checking failure
// CHECK: raboof
let i = foobar.indices
let r = lazy(i).reverse()
for a in PermutationGenerator(elements: foobar, indices: r) {
  
  print(a, appendNewline: false)
}
print("")

func isPalindrome0<
  S: CollectionType 
    where S.Index: BidirectionalIndexType, S.Generator.Element: Equatable
>(seq: S) -> Bool {
  typealias Index = S.Index

  var a = seq.indices
  var i = seq.indices
  var ir = lazy(i).reverse()
  var b = ir.generate()
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
  S: CollectionType 
  where S.Index: BidirectionalIndexType, S.Generator.Element: Equatable
>(seq: S) -> Bool {

  var a = PermutationGenerator(elements: seq, indices: seq.indices)
  var b = lazy(seq).reverse().generate()
  for nextChar in a {
    if nextChar != b.next()! {
      return false
    }
  }
  return true
}

func isPalindrome1_5<
  S: CollectionType 
  where S.Index: BidirectionalIndexType, S.Generator.Element == S.Generator.Element, S.Generator.Element: Equatable
>(seq: S) -> Bool {

  var b = lazy(seq).reverse().generate()
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
// BidirectionalIndexType traversal finally pays off!
func isPalindrome2<
  S: CollectionType 
    where S.Index: BidirectionalIndexType, S.Generator.Element: Equatable
>(seq: S) -> Bool {

  var b = seq.startIndex, e = seq.endIndex

  while (b != e) {
    if (b == --e) { 
      break
    }
    if seq[b++] != seq[e] {
      return false
    }
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
  S: CollectionType 
  where S.Index: BidirectionalIndexType, S.Generator.Element: Equatable
>(seq: S) -> Bool {
  typealias Index = S.Index

  var a = PermutationGenerator(elements: seq, indices: seq.indices)
  // FIXME: separate ri from the expression below pending
  // <rdar://problem/15772601> Type checking failure
  var i = seq.indices
  let ri = lazy(i).reverse()
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

struct SequenceOnly<T: SequenceType> : SequenceType {
  var base: T
  func generate() -> T.Generator { return base.generate() }
}

func testUnderestimateCount() {
  // CHECK: testing underestimateCount
  print("testing underestimateCount")
  // CHECK-NEXT: random access: 4
  print("random access: \(array.underestimateCount())")
  // CHECK-NEXT: bidirectional: 5
  print("bidirectional: \(dict.underestimateCount())")
  // CHECK-NEXT: SequenceType only: 0
  let s = SequenceOnly(base: array)
  print("SequenceType only: \(s.underestimateCount())")
}
testUnderestimateCount()

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

// CHECK: all done.
print("all done.")
