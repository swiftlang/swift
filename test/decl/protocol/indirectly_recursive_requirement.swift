// RUN: %target-typecheck-verify-swift

protocol Incrementable  {
  func successor() -> Self
}

protocol _ForwardIndex  {
  associatedtype Distance  = MyInt // expected-note{{declared here}}
}

protocol ForwardIndex : _ForwardIndex {
}

protocol _BidirectionalIndex : _ForwardIndex {
  func predecessor() -> Self
}

protocol BidirectionalIndex : ForwardIndex, _BidirectionalIndex {
}

protocol _RandomAccessIndex : _BidirectionalIndex {
  associatedtype Distance // expected-warning{{redeclaration of associated type 'Distance}}
}

protocol RandomAccessIndex 
  : BidirectionalIndex, _RandomAccessIndex {}

struct MyInt : RandomAccessIndex
{
  typealias Distance = MyInt

  func predecessor() -> MyInt {
  	return self
  }
}
