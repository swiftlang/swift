// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/77924
func foo<T>(_ x: (T) -> Void) {
  (a: 0, b: x).b(0) // expected-error {{cannot convert value of type 'Int' to expected argument type 'T'}}
}

func bar<T>(_ x: T) {} // expected-note {{generic parameters are always considered '@escaping'}}

func baz(_ fn: () -> Void) {
  (a: 0, b: bar).b(fn) // expected-error {{converting non-escaping parameter 'fn' to generic parameter 'T' may allow it to escape}}
}
