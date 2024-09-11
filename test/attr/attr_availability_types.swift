// RUN: %target-typecheck-verify-swift -print-diagnostic-groups

// Test that availability analysis traverses our type representations.

@available(*, deprecated)
class D {}

protocol P<T1> {
  associatedtype T1 = D // expected-warning {{'D' is deprecated [availability_deprecated]}}
  associatedtype T2: P where T2: D // expected-warning {{'D' is deprecated [availability_deprecated]}}
}

class C<T1>: D { // expected-warning {{'D' is deprecated [availability_deprecated]}}
  struct Nested<T2> {
    let d: D // expected-warning {{'D' is deprecated [availability_deprecated]}}
  }
}
extension C<D> {} // expected-warning {{'D' is deprecated [availability_deprecated]}}

func f<each T>(
  _: (
    D, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    (D), // expected-warning {{'D' is deprecated [availability_deprecated]}}
    C<D> // expected-warning {{'D' is deprecated [availability_deprecated]}}
      .Nested<D>, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    () -> D, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    (inout D) -> Void, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    (D...) -> Void, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    D?, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    [D], // expected-warning {{'D' is deprecated [availability_deprecated]}}
    [Int : D], // expected-warning {{'D' is deprecated [availability_deprecated]}}
    // FIXME: Emitted twice.
    some P<D>, // expected-warning 2 {{'D' is deprecated [availability_deprecated]}}
    any P<D>, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    any C<D> & P, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    D.Type, // expected-warning {{'D' is deprecated [availability_deprecated]}}
    repeat (D, each T) // expected-warning {{'D' is deprecated [availability_deprecated]}}
  ),
  _: @escaping (D) -> Void // expected-warning {{'D' is deprecated [availability_deprecated]}}
)
where repeat each T: D {} // expected-warning {{'D' is deprecated [availability_deprecated]}}
