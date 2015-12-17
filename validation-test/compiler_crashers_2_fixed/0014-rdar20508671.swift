// RUN: not %target-swift-frontend %s -parse

protocol MyIteratorProtocol {
  typealias Element
  mutating func next() -> Element?
}

protocol MySequence {
  typealias Iterator : MyIteratorProtocol
  func iterator() -> Iterator
}

protocol MyCollectionDefaults : MySequence {}
extension MyCollectionDefaults {
  final func iterator() -> DefaultIterator<Self> {
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
  FooIteratorWrapper(c.iterator())
}
