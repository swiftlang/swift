// RUN: %target-typecheck-verify-swift

protocol P<A>: AnyObject {
  associatedtype A: P2
  var x: A.A2 { get set }
}

protocol P2 {
  associatedtype A2
  var x: A2 { get }
}

func test<T: P2>(x: T.A2, y: any P<T>, z: any P2) {
  y.x = x // Ok
  y.x = z.x // expected-error {{cannot assign value of type 'Any' to type 'T.A2'}}
}
