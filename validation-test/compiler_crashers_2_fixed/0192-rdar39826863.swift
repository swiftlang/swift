// RUN: %target-swift-frontend -typecheck %s

protocol Tuple {
  associatedtype Head
  associatedtype Tail : Tuple
}

extension Pair : Tuple where Second : Tuple {
  typealias Head = First
  typealias Tail = Second
}

protocol HomogeneousTuple : Tuple, Collection
  where Tail : HomogeneousTuple, Head == Tail.Head {}

extension HomogeneousTuple {
  typealias Element = Head
  typealias Index = Int

  var startIndex: Int { return 0 }
  var endIndex: Int { return 0 }
  func index(after i: Int) -> Int { return i + 1 }

  subscript(n: Int) -> Head {
      fatalError()
  }
}

extension Pair : Sequence, Collection, HomogeneousTuple
  where Second : HomogeneousTuple, First == Second.Head {
  typealias Iterator = IndexingIterator<Pair<Head, Tail>>
}

struct Pair<First, Second> {}
