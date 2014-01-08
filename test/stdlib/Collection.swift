// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter
struct X : Collection {
  typealias Element = UnicodeScalar
  typealias IndexType = Int
  var msg: String

  init(msg: String) { self.msg = msg }
  func startIndex() -> IndexType {
    return 0
  }
  func endIndex() -> IndexType {
    return msg.length
  }
  func __getitem__(i: IndexType) -> Element { return msg[i] }

  func generate() -> ContainedStream<X> {
    return ContainedStream(self)
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
let r = Reverse(indices(foobar))
for a in IndexedStream(foobar, r) {
  
  print(a)
}
println("")

func isPalindrome0<
  S: Collection 
    where S.IndexType: BidirectionalIndex, S.StreamType.Element: Equatable
>(seq: S) -> Bool {
  typealias IndexType = S.IndexType

  var a = indices(seq)
  var ir = Reverse(indices(seq))
  var b = ir.generate()
  for i in a {
    if seq.__getitem__(i) != seq.__getitem__(b.next()!) {
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
  where S.IndexType: BidirectionalIndex, S.StreamType.Element: Equatable
>(seq: S) -> Bool {

  var a = IndexedStream(seq, indices(seq))
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
  where S.IndexType: BidirectionalIndex, S.StreamType.Element == S.StreamType.Element, S.StreamType.Element: Equatable
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
    where S.IndexType: BidirectionalIndex, S.StreamType.Element: Equatable
>(seq: S) -> Bool {

  var b = seq.startIndex(), e = seq.endIndex()

  while (b != e) {
    if (b == --e) { 
      break
    }
    if seq.__getitem__(b++) != seq.__getitem__(e) {
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
  where S.IndexType: BidirectionalIndex, S.StreamType.Element: Equatable
>(seq: S) -> Bool {
  typealias IndexType = S.IndexType

  var a = IndexedStream(seq, indices(seq))
  // FIXME: separate ri from the expression below pending
  // <rdar://problem/15772601> Type checking failure
  let ri = Reverse(indices(seq))
  var b = IndexedStream(seq, ri)
  for nextChar in a {
    if nextChar != b.next()! {
      return false
    }
  }
  return true
}
