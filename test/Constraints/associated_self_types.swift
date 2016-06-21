// RUN: %target-parse-verify-swift

protocol MyIteratorProtocol {
  associatedtype Element
  func next() -> Element?
}

protocol MySequence {
  associatedtype Iterator : MyIteratorProtocol
}

protocol MyCollection : MySequence {
  var startIndex: Iterator { get }
}

protocol P : MyCollection {
  init()
}
postfix operator ~>> {}

postfix func ~>> <_Self : MySequence, A : P where _Self.Iterator.Element == A.Iterator.Element>(_:_Self) -> A {
  return A()
}

protocol _ExtendedSequence : MySequence {
  postfix func ~>> <A : P where Self.Iterator.Element == A.Iterator.Element>(s: Self) -> A
}

struct MyRangeIterator<T> : MyIteratorProtocol {
  func next() -> T? { return nil }
}

struct MyRange<T> : _ExtendedSequence {
  typealias Element = T
  typealias Iterator = MyRangeIterator<T>
}

protocol Q : MySequence {
  func f<QS : MySequence where QS.Iterator.Element == Self.Iterator.Element>(_ x: QS)
}

struct No<NT> : MyIteratorProtocol {
  func next() -> NT? {
    return .none
  }
}

class X<XT> : Q {
  typealias Iterator = No<XT>
  
  func f<SX : MySequence where SX.Iterator.Element == X.Iterator.Element>(_ x: SX) {
  }
  
  func makeIterator() -> No<XT> {
    return No<XT>()
  }
}
