// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: %target-typecheck-verify-swift -disable-experimental-associated-type-inference

public protocol LazySequenceProtocol: Sequence {
  associatedtype Elements: Sequence = Self where Elements.Element == Element
}

public struct S<Base: Sequence>: LazySequenceProtocol {
  public struct Iterator: IteratorProtocol {
    public mutating func next() -> Base.Element? {
      fatalError()
    }
  }

  public func makeIterator() -> Iterator {
    fatalError()
  }
}

let x: Int.Type = S<Array<Int>>.Element.self
