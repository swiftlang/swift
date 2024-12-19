// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/60619
do {
  protocol P {
    associatedtype A
  }
  struct S {
    subscript<T: P>(_: T) -> T {
      get {} set {}
    }
  }

  func takesInOut(_: inout any P) {}

  var s: S
  let p: any P

  let _ = s[p]
  s[p] = p // expected-error {{cannot assign through subscript: 's' is immutable}}
  takesInOut(&s[p]) // expected-error {{cannot pass immutable value as inout argument: 's' is immutable}}
}
