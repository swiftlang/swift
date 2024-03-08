// RUN: %target-typecheck-verify-swift

public protocol FakeCopyable {}

extension Int: FakeCopyable {}

// Remove both 'FakeCopyable' requirements below and 
// inference for Element in Hello succeeds.

public protocol MyIteratorProtocol<Element> {
  associatedtype Element : FakeCopyable

  mutating func next() -> Element?
}

public protocol MySequence<Element> {
  associatedtype Element : FakeCopyable

  associatedtype Iterator: MyIteratorProtocol where Iterator.Element == Element

  __consuming func makeIterator() -> Iterator

  func _customContainsEquatableElement(
    _ element: Element
  ) -> Bool?
}

extension MySequence {
  @usableFromInline
  func _customContainsEquatableElement(
    _ element: Iterator.Element
  ) -> Bool? { return nil }
}

extension MySequence where Self.Iterator == Self {
  @inlinable
  public __consuming func makeIterator() -> Self {
    return self
  }
}

// ------------

internal struct Hello {}

extension Hello: MySequence, MyIteratorProtocol {
  // Inferred:
  // typealias Element = Int

  internal mutating func next() -> Int? {
    return nil
  }
}
