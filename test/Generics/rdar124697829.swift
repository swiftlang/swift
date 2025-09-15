// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.1-abi-triple

// This is a generics test, but only IRGen exercised the substitution of
// an abstract conformance with a type parameter -- in the type checker and
// SIL, we only deal with archetypes.

public protocol IteratorProtocol {
  associatedtype Element
}

public protocol Sequence {
  associatedtype Iterator: IteratorProtocol
  associatedtype Element where Element == Iterator.Element
}

public struct IndexingIterator<S: Sequence>: IteratorProtocol {
  public typealias Element = S.Element
}

public struct Array<Element>: Sequence {
  public typealias Iterator = IndexingIterator<Self>
}

public protocol Publisher {
  associatedtype Output
}

public struct SequencePublisher<Elements: Sequence>: Publisher {
  public typealias Output = Elements.Element
}

public struct Map<Pub: Publisher, Output>: Publisher {}

public func foo<T>(_: T) -> some Publisher {
  bar(SequencePublisher<Array<T>>())
}

public func bar<Pub: Publisher>(_: Pub) -> some Publisher {
  Map<Pub, Pub.Output>()
}

