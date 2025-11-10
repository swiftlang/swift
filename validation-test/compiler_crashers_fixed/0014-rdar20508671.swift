// RUN: not %target-swift-frontend %s -typecheck

protocol MyIteratorProtocol {
  typealias Element
  mutating func next() -> Element?
}

protocol MySequence {
  typealias Iterator : MyIteratorProtocol
  func makeIterator() -> Iterator
}

protocol MyCollectionDefaults : MySequence {}
extension MyCollectionDefaults {
  final func makeIterator() -> DefaultIterator<Self> {
    return DefaultIterator()
  }
}

protocol MyCollection
  : MySequence, MyCollectionDefaults {}

struct DefaultIterator<C : MyCollectionDefaults> : MyIteratorProtocol {
  mutating func next() -> C.Iterator.Element {
    fatalError("")
  }
}

struct FooIteratorWrapper<Base : MyIteratorProtocol> {
  init(_ base: Base) {}
}

func f<C : MyCollection>(c: C) {
  FooIteratorWrapper(c.makeIterator())
}
