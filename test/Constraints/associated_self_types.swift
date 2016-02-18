// RUN: %target-parse-verify-swift

protocol P : Collection {
  init()
}
postfix operator ~>> {}

postfix func ~>> <_Self : Sequence, A : P where _Self.Iterator.Element == A.Iterator.Element>(_:_Self) -> A {
  return A()
}

protocol _ExtendedSequence : Sequence {
  postfix func ~>> <A : P where Self.Iterator.Element == A.Iterator.Element>(s: Self) -> A
}

extension Range : _ExtendedSequence {
}

protocol Q : Sequence {
  func f<QS : Sequence where QS.Iterator.Element == Self.Iterator.Element>(x: QS)
}

struct No<NT> : IteratorProtocol {
  func next() -> NT? {
    return .none
  }
}

class X<XT> : Q {
  typealias Iterator = No<XT>
  
  func f<SX : Sequence where SX.Iterator.Element == X.Iterator.Element>(x: SX) {
  }
  
  func iterator() -> No<XT> {
    return No<XT>()
  }
}
