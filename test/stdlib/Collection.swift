// RUN: %target-run-simple-swift | FileCheck %s

struct X : Collection {
  typealias Element = String.GeneratorType.Element
  typealias IndexType = String.IndexType
  var msg: String

  init(_ msg: String) { self.msg = msg }
  var startIndex: IndexType {
    return msg.startIndex
  }
  var endIndex: IndexType {
    return msg.endIndex
  }
  subscript(i: IndexType) -> Element { return msg[i] }

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
let r = Reverse(i)
for a in IndexedGenerator(sequence: foobar, indices: r) {
  
  print(a)
}
println("")

func isPalindrome0<
  S: Collection 
    where S.IndexType: BidirectionalIndex, S.GeneratorType.Element: Equatable
>(seq: S) -> Bool {
  typealias IndexType = S.IndexType

  var a = indices(seq)
  var i = indices(seq)
  var ir = Reverse(i)
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
  S: Collection 
  where S.IndexType: BidirectionalIndex, S.GeneratorType.Element: Equatable
>(seq: S) -> Bool {

  var a = IndexedGenerator(sequence: seq, indices: indices(seq))
  var b = Reverse(seq).generate()
  for nextChar in a {
    if nextChar != b.next()! {
      return false
    }
  }
  return true
}

func isPalindrome1_5<
  S: Collection 
  where S.IndexType: BidirectionalIndex, S.GeneratorType.Element == S.GeneratorType.Element, S.GeneratorType.Element: Equatable
>(seq: S) -> Bool {

  var b = Reverse(seq).generate()
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
// BidirectionalIndex traversal finally pays off!
func isPalindrome2<
  S: Collection 
    where S.IndexType: BidirectionalIndex, S.GeneratorType.Element: Equatable
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
  S: Collection 
  where S.IndexType: BidirectionalIndex, S.GeneratorType.Element: Equatable
>(seq: S) -> Bool {
  typealias IndexType = S.IndexType

  var a = IndexedGenerator(sequence: seq, indices: indices(seq))
  // FIXME: separate ri from the expression below pending
  // <rdar://problem/15772601> Type checking failure
  var i = indices(seq)
  let ri = Reverse(i)
  var b = IndexedGenerator(sequence: seq, indices: ri)
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

func testCountElements() {
  // CHECK: testing countElements
  println("testing countElements")
  // CHECK-NEXT: random access: 4
  println("random access: \(countElements(array))")
  // CHECK-NEXT: bidirectional: 5
  println("bidirectional: \(countElements(dict))")
}
testCountElements()

func testUnderestimateCount() {
  // CHECK: testing underestimateCount
  println("testing underestimateCount")
  // CHECK-NEXT: random access: 4
  println("random access: \(underestimateCount(array))")
  // CHECK-NEXT: bidirectional: 5
  println("bidirectional: \(underestimateCount(dict))")
  // CHECK-NEXT: Sequence only: 0
  let s = SequenceOf(array)
  println("Sequence only: \(underestimateCount(s))")
}
testUnderestimateCount()

// CHECK: all done.
println("all done.")
