// RUN: %target-typecheck-verify-swift

// Test that availability analysis traverses our type representations.

@available(*, deprecated)
class D {}

protocol P<T1> {
  associatedtype T1 = D // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
  associatedtype T2: P where T2: D // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
}

class C<T1>: D { // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
  struct Nested<T2> {
    let d: D // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
  }
}
extension C<D> {} // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}

func f<each T>(
  _: (
    D, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    (D), // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    C<D> // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
      .Nested<D>, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    () -> D, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    (inout D) -> Void, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    (D...) -> Void, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    D?, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    [D], // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    [Int : D], // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    // FIXME: Emitted twice.
    some P<D>, // expected-warning 2 {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    any P<D>, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    any C<D> & P, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    D.Type, // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
    repeat (D, each T) // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
  ),
  _: @escaping (D) -> Void // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
)
where repeat each T: D {} // expected-warning {{'D' is deprecated}}{{documentation-file=deprecated-declaration}}
