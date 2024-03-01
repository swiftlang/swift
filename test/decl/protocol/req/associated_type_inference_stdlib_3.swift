// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: not %target-typecheck-verify-swift -disable-experimental-associated-type-inference

struct FooIterator<T: Sequence>: IteratorProtocol {
  typealias Element = T.Element

  mutating func next() -> Element? { fatalError() }
}

struct FooSequence<Element>: Sequence {
  func makeIterator() -> FooIterator<Self> { fatalError() }
}
