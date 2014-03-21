// RUN: %swift -parse %s -verify

protocol P : Collection {
  init()
}
operator postfix ~>> {}

@postfix func ~>> <_Self: Sequence, A: P where _Self.GeneratorType.Element == A.GeneratorType.Element>(_:_Self) -> A {
  return A()
}

protocol _ExtendedSequence : Sequence {
  //typealias GeneratorType = RangeGenerator<T>
  @postfix func ~>> <A: P where Self.GeneratorType.Element == A.GeneratorType.Element>(s: Self) -> A
}

extension Range : _ExtendedSequence {
}

protocol Q : Sequence {
  func f<QS: Sequence where QS.GeneratorType.Element == Self.GeneratorType.Element>(x: QS)
}

struct No<NT> : Generator {
  func next() -> NT? {
    return .None
  }
}

class X<XT> : Q {
  typealias GeneratorType = No<XT>
  
  func f<SX: Sequence where SX.GeneratorType.Element == X.GeneratorType.Element>(x: SX) {
  }
  
  func generate() -> No<XT> {
    return No<XT>()
  }
}
