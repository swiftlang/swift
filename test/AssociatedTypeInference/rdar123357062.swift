// RUN: %target-typecheck-verify-swift

extension LazySequenceProtocol {
  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  func filtering(
     while predicate: @escaping (Self.Element) -> Bool
  ) -> some (Sequence<Self.Element> & LazySequenceProtocol) {
    LazyFilteringSequence(base: self, predicate: predicate)
  }
}

struct LazyFilteringSequence<S: Sequence>: LazySequenceProtocol {
  struct Iterator: IteratorProtocol {
    var iterator: S.Iterator
    var predicate: (S.Element) -> Bool

    mutating func next() -> S.Element? {
      nil
    }
  }

  var base: S
  var predicate: (S.Element) -> Bool

  func makeIterator() -> Iterator {
    Iterator(iterator: self.base.makeIterator(), predicate: self.predicate)
  }
}
