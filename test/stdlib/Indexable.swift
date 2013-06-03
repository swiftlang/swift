// RUN: %swift -i %s | FileCheck %s
struct X : Indexable {
  typealias Element = Char
  typealias Index = Int
  var msg: String

  constructor(msg: String) { this.msg = msg }
  func begin() -> Index { return 0 }
  func end() -> Index { return msg.length }
  func __getitem__(i: Index) -> Element { return msg[i] }
}

var foobar = X("foobar")

// CHECK: foobar
for a in IndexableEnumerator(foobar, Range(foobar.begin(), foobar.end())) {
  print(a)
}
println("")

// CHECK: raboof
for a in IndexableEnumerator(foobar, ReverseRange(foobar.begin(), foobar.end())) {
  print(a)
}
println("")

func isPalindrome0<
  S: Indexable 
    requires S.Index: Bidirectional, S.Element: Equatable
    , S.Element.Self == S.Element, S.Index.Self == S.Index
>(seq: S) -> Bool {
  typealias Index : Bidirectional = S.Index

  var a = Range<Index>(seq.begin(), seq.end()).getEnumeratorType()
  var b = ReverseRange<Index>(seq.begin(), seq.end()).getEnumeratorType()
  for i in a {
    if seq.__getitem__(i) != seq.__getitem__(b.next()) {
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
  S: Indexable 
    requires S.Index: Bidirectional, S.Element: Equatable
    , S.Element.Self == S.Element, S.Index.Self == S.Index
>(seq: S) -> Bool {

  var a = ForwardIndexableEnumerator(seq)
  var b = ReverseRange<Index>(seq.begin(), seq.end()).getEnumeratorType()
  for c in a {
    if c != seq.__getitem__(b.next()) {
      return false
    }
  }
  return true
}

// CHECK: false
println(isPalindrome1(X("MADAMINEDENIMWILLIAM")))
// CHECK: true
println(isPalindrome1(X("MADAMINEDENIMADAM")))
