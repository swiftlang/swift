// RUN: %target-parse-verify-swift

protocol Incrementable  {
  func successor() -> Self
}

protocol _ForwardIndexType  {
  typealias Distance  = MyInt
}

protocol ForwardIndex : _ForwardIndexType {
}

protocol _BidirectionalIndexType : _ForwardIndexType {
  func predecessor() -> Self
}

protocol BidirectionalIndex : ForwardIndex, _BidirectionalIndexType {
}

protocol _RandomAccessIndexType : _BidirectionalIndexType {
  typealias Distance
}

protocol RandomAccessIndex 
  : BidirectionalIndex, _RandomAccessIndexType {}

struct MyInt : RandomAccessIndex
{
  typealias Distance = MyInt

  func predecessor() -> MyInt {
  	return self
  }
}
