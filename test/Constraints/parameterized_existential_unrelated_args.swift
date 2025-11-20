// RUN: %target-typecheck-verify-swift

// We used to accept this in Swift 6.0, but it's incorrect;
// the primary associated types of a derived protocol might
// be completely unrelated to those of a base protocol.

protocol P<A> : Q {
  associatedtype A
}

protocol Q<B> {
  associatedtype B

  func getB() -> B
}

struct S<T>: P {
  typealias A = T
  typealias B = Array<T>

  var t: T

  func getB() -> Array<T> {
    return [t]
  }
}

var p: any P<String> = S<String>(t: "hello world")
var q: any Q<String> = p // expected-error {{cannot convert value of type 'any P<String>' to specified type 'any Q<String>'}}

// Previously we accepted the above conversion, and then getB()
// would return something that was dynamically Array<String>
// and not String as expected.
// print(q.getB())


// However, this is OK -- the two A's have the same name, so by the
// semantics of the generics system they must be equivalent as type
// parameters.

protocol P1<A> {
  associatedtype A
}

protocol P2<A> {
  associatedtype A
}

protocol P3<A>: P1, P2 {
  associatedtype A
}

func f<T>(_ value: any P3<T>) -> (any P1<T>, any P2<T>) {
  return (value, value)
}
