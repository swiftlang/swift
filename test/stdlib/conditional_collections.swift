// RUN: %target-run-simple-swift
// REQUIRES: executable_test


struct X: BidirectionalCollection {
  var startIndex: Int { return 0 }
  var endIndex: Int { return 10 }
  subscript(position: Int) -> String { return "element" }
  subscript(range: Range<Int>) -> X { return X() }
  func index(after i: Int) -> Int { return i + 1 }
  func index(before i: Int) -> Int { return i - 1 }
}
struct A<C: Collection>: Collection {
  let c: C
  var startIndex: C.Index { return c.startIndex }
  var endIndex: C.Index { return c.endIndex }
  subscript(position: C.Index) -> C.Element { return c[position] }
  subscript(range: Range<C.Index>) -> A<C.SubSequence> {
    return A<C.SubSequence>(c: c[range])
  }
  func index(after i: C.Index) -> C.Index { return c.index(after: i) }
}

extension A: BidirectionalCollection where C: BidirectionalCollection {
  func index(before i: C.Index) -> C.Index { return c.index(before: i) }
}

// SR-8022
func sr8022() {
  var c = A(c: X())
  _ = c.popLast()
  _ = c.removeLast()
  c.removeLast(2)
  _ = c.dropLast(2)
  _ = c.suffix(2)
}

sr8022()
