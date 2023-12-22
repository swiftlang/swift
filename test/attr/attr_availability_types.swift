// RUN: %target-typecheck-verify-swift

// Test that availability analysis traverses our type representations.

@available(*, deprecated)
class D {}

protocol P<T1> {
  associatedtype T1 = D // expected-warning {{'D' is deprecated}}
  associatedtype T2: P where T2: D // expected-warning {{'D' is deprecated}}
}

class C<T1>: D { // expected-warning {{'D' is deprecated}}
  struct Nested<T2> {
    let d: D // expected-warning {{'D' is deprecated}}
  }
}
extension C<D> {} // expected-warning {{'D' is deprecated}}

func f<each T>(
  _: (
    D, // expected-warning {{'D' is deprecated}}
    (D), // expected-warning {{'D' is deprecated}}
    C<D> // expected-warning {{'D' is deprecated}}
      .Nested<D>, // expected-warning {{'D' is deprecated}}
    () -> D, // expected-warning {{'D' is deprecated}}
    (inout D) -> Void, // expected-warning {{'D' is deprecated}}
    (D...) -> Void, // expected-warning {{'D' is deprecated}}
    D?, // expected-warning {{'D' is deprecated}}
    [D], // expected-warning {{'D' is deprecated}}
    [Int : D], // expected-warning {{'D' is deprecated}}
    // FIXME: Emitted twice.
    some P<D>, // expected-warning 2 {{'D' is deprecated}}
    any P<D>, // expected-warning {{'D' is deprecated}}
    any C<D> & P, // expected-warning {{'D' is deprecated}}
    D.Type, // expected-warning {{'D' is deprecated}}
    repeat (D, each T) // expected-warning {{'D' is deprecated}}
  ),
  _: @escaping (D) -> Void // expected-warning {{'D' is deprecated}}
)
where repeat each T: D {} // expected-warning {{'D' is deprecated}}
