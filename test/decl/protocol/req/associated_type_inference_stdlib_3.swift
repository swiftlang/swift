// RUN: not %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: not %target-typecheck-verify-swift -disable-experimental-associated-type-inference

// FIXME: Get this passing with -enable-experimental-associated-type-inference again.

struct FooIterator<T: Sequence>: IteratorProtocol {
  typealias Element = T.Element

  mutating func next() -> Element? { fatalError() }
}

struct FooSequence<Element>: Sequence {
  func makeIterator() -> FooIterator<Self> { fatalError() }
}
