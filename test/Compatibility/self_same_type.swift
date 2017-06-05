// RUN: %target-typecheck-verify-swift -swift-version 3

protocol P {
  associatedtype T
}

protocol Q {
  func foo<T: P>(_: T, _: T.T) where T.T == Self
}

class C1: Q {
  func foo<T: P>(_: T, _: C1) where T.T == C1 {} // expected-warning{{instance method 'foo' in non-final class 'C1' cannot be used to satisfy requirement instance method 'foo' (in protocol 'Q') due to same-type requirement involving 'Self'}}}}
    // expected-note@-1{{consider weakening the same-type requirement 'T.T' == 'C1' to a superclass requirement}}{{41-43=:}}
}

final class C2: Q {
  func foo<T: P>(_: T, _: C2) where T.T == C2 {}
}

class C3: Q {
  func foo<T: P>(_: T, _: C3) where T.T: C3 {}
}
