// RUN: %swift -parse %s -verify

protocol Incrementable  {
  func succ() -> Self
}

protocol _ForwardIndex  { // expected-error {{type may not reference itself as a requirement}}
  typealias DistanceType  = MyInt
}

protocol ForwardIndex : _ForwardIndex {
}

protocol _BidirectionalIndex : _ForwardIndex { // expected-note {{type 'MyInt' does not conform to inherited protocol '_ForwardIndex'}}
  func pred() -> Self
}

protocol BidirectionalIndex : ForwardIndex, _BidirectionalIndex { // expected-note {{type 'MyInt' does not conform to inherited protocol '_BidirectionalIndex'}}
}

protocol _RandomAccessIndex : _BidirectionalIndex {
  typealias DistanceType : _NumericOperations // expected-note {{protocol requires nested type 'DistanceType'}}
}

protocol RandomAccessIndex : BidirectionalIndex, _RandomAccessIndex { // expected-note {{type 'MyInt' does not conform to inherited protocol '_RandomAccessIndex'}}
}


protocol _NumericOperations {
}

struct MyInt : RandomAccessIndex // expected-error {{type 'MyInt' does not conform to protocol '_RandomAccessIndex'}}
{
  
}
