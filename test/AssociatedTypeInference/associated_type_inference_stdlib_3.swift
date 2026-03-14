// RUN: %target-typecheck-verify-swift

struct FooIterator<T: Sequence>: IteratorProtocol {
  typealias Element = T.Element

  mutating func next() -> Element? { fatalError() }
}

struct FooSequence<Element>: Sequence {
  func makeIterator() -> FooIterator<Self> { fatalError() }
}
