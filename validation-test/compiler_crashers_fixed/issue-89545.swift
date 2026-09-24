// https://github.com/swiftlang/swift/issues/89545
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple
protocol P {
  func makeA() -> A
  associatedtype A: Q
}
protocol Q {}
struct S1<each T>: P where repeat each T: P {
  let t: (repeat each T)
  func makeA() -> S2<repeat (each T).A> {
    return A(t: repeat (each t).makeA())
    // expected-error@-1:17 {{cannot pass value pack expansion to non-pack parameter of type '(repeat each T)'}}
    // expected-note@-2:17 {{did you mean to expand the pack into a tuple?}}
  }
}
struct S2<each T>: Q where repeat each T: Q {
  let t: (repeat each T)
}
