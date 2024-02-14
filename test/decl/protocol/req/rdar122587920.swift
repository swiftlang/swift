// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference -DNEW
// RUN: %target-typecheck-verify-swift -disable-experimental-associated-type-inference -DOLD

struct S<Element> {}

extension S: Sequence, IteratorProtocol {
  mutating func next() -> Element?? {
    fatalError()
  }
}

extension S: Collection {
  var startIndex: Int {
    fatalError()
  }

  var endIndex: Int {
    fatalError()
  }

  subscript(_ index: Int) -> Element? {
    fatalError()
  }

  func index(after index: Int) -> Int {
    fatalError()
  }
}

// The old behavior didn't make much sense.

#if NEW
let x: S<Int>.Type = S<Int>.Iterator.self
#elseif OLD
let x: IndexingIterator<S<Int>>.Type = S<Int>.Iterator.self
#endif