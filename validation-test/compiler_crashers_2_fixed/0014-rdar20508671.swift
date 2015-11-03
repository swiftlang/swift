// RUN: not %target-swift-frontend %s -parse

protocol MyIteratorProtocol {
  typealias Element
  mutating func next() -> Element?
}

protocol MySequenceType {
  typealias Iterator : MyIteratorProtocol
  func generate() -> Iterator
}

protocol MyCollectionDefaultsType : MySequenceType {}
extension MyCollectionDefaultsType {
  final func generate() -> DefaultIterator<Self> {
    return DefaultIterator()
  }
}

protocol MyCollectionType
  : MySequenceType, MyCollectionDefaultsType {}

struct DefaultIterator<C : MyCollectionDefaultsType> : MyIteratorProtocol {
  mutating func next() -> C.Iterator.Element {
    fatalError("")
  }
}

struct FooIteratorWrapper<Base : MyIteratorProtocol> {
  init(_ base: Base) {}
}

func f<C : MyCollectionType>(c: C) {
  FooIteratorWrapper(c.generate())
}
