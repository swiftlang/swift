// RUN: %target-typecheck-verify-swift

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
postfix operator ~>>

postfix func ~>> <_Self : MySequence, A : P>(_:_Self) -> A
  where _Self.Iterator.Element == A.Iterator.Element {
  return A()
}

protocol _ExtendedSequence : MySequence {
  static postfix func ~>> <A : P>(s: Self) -> A
    where Self.Iterator.Element == A.Iterator.Element
}

struct MyRangeIterator<T> : MyIteratorProtocol {
  func next() -> T? { return nil }
}

struct MyRange<T> : _ExtendedSequence {
  typealias Element = T
  typealias Iterator = MyRangeIterator<T>
}

protocol Q : MySequence {
  func f<QS : MySequence>(_ x: QS)
    where QS.Iterator.Element == Self.Iterator.Element
}

struct No<NT> : MyIteratorProtocol {
  func next() -> NT? {
    return .none
  }
}

class X<XT> : Q {
  typealias Iterator = No<XT>
  
  func f<SX : MySequence>(_ x: SX)
    where SX.Iterator.Element == X.Iterator.Element {
  }
  
  func makeIterator() -> No<XT> {
    return No<XT>()
  }
}
