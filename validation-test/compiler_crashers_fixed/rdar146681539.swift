// RUN: %target-typecheck-verify-swift

protocol Q {
  func foo()
}

protocol P<T>: AnyObject {
  associatedtype T: Q
}

final class C<T: Q>: P {
}

struct S: Q {
  func foo() {}
}

// This used to return an ErrorType and crash

func testit() -> P<S> {  // expected-warning {{use of protocol 'P' as a type must be written 'any P'}}
  return C<S>()
}
