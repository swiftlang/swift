// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

// https://github.com/swiftlang/swift/issues/77840
do {
  struct G<T> {}

  protocol P {
    associatedtype A where A == Undefined
    // expected-error@-1 {{cannot find type 'Undefined' in scope}}
    associatedtype B where B == G<Undefined>
    // expected-error@-1 {{cannot find type 'Undefined' in scope}}

    func fooTakesA(_: A)
    func fooTakesB(_: B)

    func fooReturnsA() -> A
    func fooReturnsB() -> B
  }

  let p: any P
  let _ = p.fooTakesA
  // expected-error@-1 {{member 'fooTakesA' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  let _ = p.fooTakesB
  // expected-error@-1 {{member 'fooTakesB' cannot be used on value of type 'any P'; consider using a generic constraint instead}}
  let _ = p.fooReturnsA()
  let _ = p.fooReturnsB()
}
