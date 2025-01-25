// RUN: %target-typecheck-verify-swift -print-diagnostic-groups

// Test that availability analysis traverses our type representations.

@available(*, deprecated)
class D {}

protocol P<T1> {
  associatedtype T1 = D // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
  associatedtype T2: P where T2: D // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
}

class C<T1>: D { // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
  struct Nested<T2> {
    let d: D // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
  }
}
extension C<D> {} // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}

func f<each T>(
  _: (
    D, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    (D), // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    C<D> // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
      .Nested<D>, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    () -> D, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    (inout D) -> Void, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    (D...) -> Void, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    D?, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    [D], // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    [Int : D], // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    // FIXME: Emitted twice.
    some P<D>, // expected-warning 2 {{'D' is deprecated [DeprecatedDeclaration]}}
    any P<D>, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    any C<D> & P, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    D.Type, // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
    repeat (D, each T) // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
  ),
  _: @escaping (D) -> Void // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
)
where repeat each T: D {} // expected-warning {{'D' is deprecated [DeprecatedDeclaration]}}
