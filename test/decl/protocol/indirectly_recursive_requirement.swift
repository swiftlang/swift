// RUN: %target-parse-verify-swift

protocol Incrementable  {
  func successor() -> Self
}

protocol _ForwardIndexType  {
  typealias Distance  = MyInt
}

protocol ForwardIndexType : _ForwardIndexType {
}

protocol _BidirectionalIndexType : _ForwardIndexType {
  func predecessor() -> Self
}

protocol BidirectionalIndexType : ForwardIndexType, _BidirectionalIndexType {
}

protocol _RandomAccessIndexType : _BidirectionalIndexType {
  typealias Distance
}

protocol RandomAccessIndexType 
  : BidirectionalIndexType, _RandomAccessIndexType {}

struct MyInt : RandomAccessIndexType
{
  typealias Distance = MyInt

  func predecessor() -> MyInt {
  	return self
  }
}
