// RUN: not %target-swift-frontend %s -typecheck

protocol P {
  associatedtype A1 : Q where A1.A2 == Self

  var a1: A1? { get set }

  func start()
}

protocol Q {
  associatedtype A2 : P where A2.A1 == Self

  func didStart(transport: A2)
}

class C<D> : P
  where D : Q, D.A2 == Self
{
  typealias A1 = D

  var a1: D? = nil
}

class C2<D> : P
  where D : Q, D.A2 == C2<D>
{
  typealias A1 = D

  var a1: D? = nil
}
