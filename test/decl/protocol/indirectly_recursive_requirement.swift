// RUN: %target-typecheck-verify-swift

protocol Incrementable  {
  func successor() -> Self
}

protocol _ForwardIndex  {
  associatedtype Distance  = MyInt
}

protocol ForwardIndex : _ForwardIndex {
}

protocol _BidirectionalIndex : _ForwardIndex {
  func predecessor() -> Self
}

protocol BidirectionalIndex : ForwardIndex, _BidirectionalIndex {
}

protocol _RandomAccessIndex : _BidirectionalIndex {
  associatedtype Distance
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
