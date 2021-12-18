import RoundTrip

protocol P {}
protocol Q {}
class C {}
class D : C, P, Q {}

public func test() {
  roundTripType(Any.self)
  roundTripType(AnyObject.self)
  roundTripType(P.self)
  roundTripType((C & P).self)
  roundTripType((P & AnyObject).self)
  roundTripType((P & Q).self)
  roundTripType((C & P & Q).self)
  roundTripType((P & Q & AnyObject).self)

  roundTripType(Any.Type.self)
  roundTripType(AnyObject.Type.self)
  roundTripType(P.Type.self)
  roundTripType((C & P).Type.self)
  roundTripType((P & AnyObject).Type.self)
  roundTripType((P & Q).Type.self)
  roundTripType((C & P & Q).Type.self)
  roundTripType((P & Q & AnyObject).Type.self)

  roundTripType(Any.Protocol.self)
  roundTripType(AnyObject.Protocol.self)
  roundTripType(P.Protocol.self)
  roundTripType((C & P).Protocol.self)
  roundTripType((P & AnyObject).Protocol.self)
  roundTripType((P & Q).Protocol.self)
  roundTripType((C & P & Q).Protocol.self)
  roundTripType((P & Q & AnyObject).Protocol.self)
}
