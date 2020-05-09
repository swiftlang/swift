// RUN: not %target-swift-frontend %s -typecheck

struct S : Sequence {
  struct Iterator : IteratorProtocol {
    mutating func next() -> Int? {
      fatalError()
    }
  }

  func makeIterator() -> Iterator {
    fatalError()
  }
}

extension S : Collection {
  typealias Index = Int

  var startIndex: Int { return 0 }
  var endIndex: Int { return 1 }
}
