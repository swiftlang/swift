// RUN: not %target-swift-frontend %s -parse

protocol MyGeneratorType {
  typealias Element
  mutating func next() -> Element?
}

protocol MySequenceType {
  typealias Generator : MyGeneratorType
  func generate() -> Generator
}

protocol MyCollectionDefaultsType : MySequenceType {}
extension MyCollectionDefaultsType {
  final func generate() -> DefaultGenerator<Self> {
    return DefaultGenerator()
  }
}

protocol MyCollectionType
  : MySequenceType, MyCollectionDefaultsType {}

struct DefaultGenerator<C : MyCollectionDefaultsType> : MyGeneratorType {
  mutating func next() -> C.Generator.Element {
    fatalError("")
  }
}

struct FooGeneratorWrapper<Base : MyGeneratorType> {
  init(_ base: Base) {}
}

func f<C : MyCollectionType>(c: C) {
  FooGeneratorWrapper(c.generate())
}
