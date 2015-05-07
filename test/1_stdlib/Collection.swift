// RUN: %target-run-simple-swift | FileCheck %s

struct X : CollectionType {
  typealias Element = String.Generator.Element
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
  print(a)
}
println("")

// FIXME: separate r from the expression below pending
// <rdar://problem/15772601> Type checking failure
// CHECK: raboof
let i = indices(foobar)
let r = lazy(i).reverse()
for a in PermutationGenerator(elements: foobar, indices: r) {
  
  print(a)
}
println("")

func isPalindrome0<
  S: CollectionType 
    where S.Index: BidirectionalIndexType, S.Generator.Element: Equatable
>(seq: S) -> Bool {
  typealias Index = S.Index

  var a = indices(seq)
  var i = indices(seq)
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
println(isPalindrome0(X("GoHangaSalamiImaLasagneHoG")))
// CHECK: true
println(isPalindrome0(X("GoHangaSalamiimalaSagnaHoG")))

func isPalindrome1<
  S: CollectionType 
  where S.Index: BidirectionalIndexType, S.Generator.Element: Equatable
>(seq: S) -> Bool {

  var a = PermutationGenerator(elements: seq, indices: indices(seq))
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
println(isPalindrome1(X("MADAMINEDENIMWILLIAM")))
// CHECK: true
println(isPalindrome1(X("MadamInEdEnImadaM")))

// CHECK: false
println(isPalindrome1_5(X("FleetoMeRemoteelF")))
// CHECK: true
println(isPalindrome1_5(X("FleetoMeReMoteelF")))

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
println(isPalindrome2(X("ZerimarRamireZ")))
// CHECK: true
println(isPalindrome2(X("ZerimaRRamireZ")))

// Test odd length
// CHECK: false
println(isPalindrome2(X("ZerimarORamireZ")))
// CHECK: true
println(isPalindrome2(X("Zerimar-O-ramireZ")))

func isPalindrome4<
  S: CollectionType 
  where S.Index: BidirectionalIndexType, S.Generator.Element: Equatable
>(seq: S) -> Bool {
  typealias Index = S.Index

  var a = PermutationGenerator(elements: seq, indices: indices(seq))
  // FIXME: separate ri from the expression below pending
  // <rdar://problem/15772601> Type checking failure
  var i = indices(seq)
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
  println("testing count")
  // CHECK-NEXT: random access: 4
  println("random access: \(count(array))")
  // CHECK-NEXT: bidirectional: 5
  println("bidirectional: \(count(dict))")
}
testCount()

struct SequenceOnly<T: SequenceType> : SequenceType {
  var base: T
  func generate() -> T.Generator { return base.generate() }
}

func testUnderestimateCount() {
  // CHECK: testing underestimateCount
  println("testing underestimateCount")
  // CHECK-NEXT: random access: 4
  println("random access: \(array.underestimateCount())")
  // CHECK-NEXT: bidirectional: 5
  println("bidirectional: \(dict.underestimateCount())")
  // CHECK-NEXT: SequenceType only: 0
  let s = SequenceOnly(base: array)
  println("SequenceType only: \(s.underestimateCount())")
}
testUnderestimateCount()

func testIsEmptyFirstLast() {
  // CHECK: testing isEmpty
  println("testing isEmpty")
  // CHECK-NEXT: true
  println((10..<10).isEmpty)
  // CHECK-NEXT: false
  println((10...10).isEmpty)
  // CHECK-NEXT: 10
  println((10..<100).first)
  // CHECK-NEXT: 99
  println(last(10..<100))
}
testIsEmptyFirstLast()

// CHECK: all done.
println("all done.")
