// RUN: %target-typecheck-verify-swift

// This is too circular to work, but it shouldn't crash.

protocol MyCollectionProtocol: Collection where Iterator == MyCollectionIterator<Self> {}
// expected-error@-1 {{circular reference}}

struct MyCollectionIterator<MyCollection: MyCollectionProtocol>: IteratorProtocol {
// expected-note@-1 3{{through reference here}}
    mutating func next() -> MyCollection.Element? {
        return nil
    }
}

struct MyCollection: MyCollectionProtocol {
    struct Element {}

    var startIndex: Int { fatalError() }
    var endIndex: Int { fatalError() }

    func index(after i: Int) -> Int { fatalError() }
    subscript(position: Int) -> Element { fatalError() }

    public func makeIterator() -> MyCollectionIterator<Self> { fatalError() }
}
