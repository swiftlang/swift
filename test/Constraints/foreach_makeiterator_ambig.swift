// RUN: %target-typecheck-verify-swift

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
  func foo() -> WrapperSequence<Self> {} // expected-note {{found this candidate}}
  func foo() -> WrapperCollection<Self> {} // expected-note {{found this candidate}}
}

func foo(_ x: some Collection) {
  // This is ambiguous since WrapperSequence & WrapperCollection are unrelated
  // types, but was previously unambiguous purely because Collection's default
  // 'makeIterator' was considered "less specialized" than WrapperSequence's
  // concrete implementation.
  for _ in x.foo() {}
  // expected-error@-1 {{ambiguous use of 'foo()'}}
}
