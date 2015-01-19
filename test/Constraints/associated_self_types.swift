// RUN: %target-parse-verify-swift

protocol P : CollectionType {
  init()
}
postfix operator ~>> {}

postfix func ~>> <_Self: SequenceType, A: P where _Self.Generator.Element == A.Generator.Element>(_:_Self) -> A {
  return A()
}

protocol _ExtendedSequence : SequenceType {
  //typealias Generator = RangeGenerator<T>
  postfix func ~>> <A: P where Self.Generator.Element == A.Generator.Element>(s: Self) -> A
}

extension Range : _ExtendedSequence {
}

protocol Q : SequenceType {
  func f<QS: SequenceType where QS.Generator.Element == Self.Generator.Element>(x: QS)
}

struct No<NT> : GeneratorType {
  func next() -> NT? {
    return .None
  }
}

class X<XT> : Q {
  typealias Generator = No<XT>
  
  func f<SX: SequenceType where SX.Generator.Element == X.Generator.Element>(x: SX) {
  }
  
  func generate() -> No<XT> {
    return No<XT>()
  }
}
