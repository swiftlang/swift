// RUN: %swift -parse %s -verify

protocol Incrementable  {
  func succ() -> Self
}

protocol _ForwardIndex  {
  typealias DistanceType  = MyInt
}

protocol ForwardIndex : _ForwardIndex {
}

protocol _BidirectionalIndex : _ForwardIndex {
  func pred() -> Self
}

protocol BidirectionalIndex : ForwardIndex, _BidirectionalIndex {
}

protocol _RandomAccessIndex : _BidirectionalIndex {
  typealias DistanceType
}

protocol RandomAccessIndex : BidirectionalIndex, _RandomAccessIndex {
}

struct MyInt : RandomAccessIndex
{
  typealias DistanceType = MyInt

  func pred() -> MyInt {
  	return self
  }
}
