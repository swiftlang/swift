// RUN: %target-typecheck-verify-swift -swift-version 6 -verify-additional-prefix swift6-
// RUN: %target-typecheck-verify-swift -swift-version 7 -verify-additional-prefix swift7-

// REQUIRES: swift7

struct WrapperSequence<Base: Collection>: Sequence {
  struct Iterator: IteratorProtocol {
    mutating func next() -> Base.Element? {}
  }
  func makeIterator() -> Iterator {}
}
struct WrapperCollection<Base: Collection> : Collection {
  typealias Element = Base.Element
  typealias Index = Base.Index

  var startIndex: Index {}
  var endIndex: Index {}

  func index(after i: Index) -> Index {}
  subscript(idx: Index) -> Element {}
}

extension Collection {
  func foo() -> WrapperSequence<Self> {} // expected-swift7-note {{found this candidate}}
  func foo() -> WrapperCollection<Self> {} // expected-swift7-note {{found this candidate}}
}

func foo(_ x: some Collection) {
  // This should be ambiguous since WrapperSequence & WrapperCollection are
  // unrelated types, but was previously unambiguous purely because Collection's
  // default 'makeIterator' was considered "less specialized" than WrapperSequence's
  // concrete implementation.
  for _ in x.foo() {}
  // expected-swift7-error@-1 {{ambiguous use of 'foo()'}}
}
