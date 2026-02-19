// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

// This is too circular to work, but it shouldn't crash.

protocol MyCollectionProtocol: Collection where Iterator == MyCollectionIterator<Self> {}
// expected-error@-1:10 {{circular reference}}

struct MyCollectionIterator<MyCollection: MyCollectionProtocol>: IteratorProtocol {
  // expected-note@-1:8 {{through reference here}}
  // expected-note@-2:66 {{through reference here}}
  // expected-error@-3 {{type 'MyCollectionIterator<MyCollection>' does not conform to protocol 'IteratorProtocol'}}
  // expected-note@-4 {{add stubs for conformance}}
    mutating func next() -> MyCollection.Element? {
    // expected-error@-1 {{'Element' is not a member type of type 'MyCollection'}}
        return nil
    }
}

struct MyCollection: MyCollectionProtocol {
// expected-error@-1 {{type 'MyCollection' does not conform to protocol 'Collection'}}
// expected-error@-2 {{type 'MyCollection' does not conform to protocol 'Sequence'}}
// expected-error@-3 {{type 'MyCollection' does not conform to protocol 'BorrowingSequence'}}
    struct Element {}

    var startIndex: Int { fatalError() }
    var endIndex: Int { fatalError() }

    func index(after i: Int) -> Int { fatalError() }
    subscript(position: Int) -> Element { fatalError() }

    public func makeIterator() -> MyCollectionIterator<Self> { fatalError() }
}
