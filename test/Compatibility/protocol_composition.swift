// RUN: %target-typecheck-verify-swift -swift-version 3 -D SWIFT3
// RUN: %target-typecheck-verify-swift -swift-version 4 -D SWIFT4

protocol P1 {
  static func p1() -> Int
}
protocol P2 {
  static var p2: Int { get }
}

// Swift3:
//   Warning for mistakenly accepted protocol composition production.
// Swift4:
//   Non-ident type in composition is always rejected.
func foo(x: P1 & Any & P2.Type?) {
  // expected-warning(SWIFT3) @-1 {{protocol-constrained type with postfix '.Type' is ambiguous and will be rejected in future version of Swift}} {{13-13=(}} {{26-26=)}}
  // expected-error(SWIFT4) @-2 {{non-protocol, non-class type 'P2.Type?' cannot be used within a protocol-constrained type}}

  let _: (P1 & P2).Type? = x // expected-error(SWIFT4) {{cannot convert value of type 'P1' to specified type '(P1 & P2).Type?'}}
  let _: (P1 & P2).Type = x! // expected-error(SWIFT4) {{cannot force unwrap value of non-optional type 'P1'}}
  let _: Int = x!.p1() // expected-error(SWIFT4) {{cannot force unwrap value of non-optional type 'P1'}}
  let _: Int? = x?.p2 // expected-error(SWIFT4) {{cannot use optional chaining on non-optional value of type 'P1'}}
}

// Type expression
func bar() -> ((P1 & P2)?).Type {
  let x = (P1 & P2?).self
  // expected-warning(SWIFT3) @-1 {{protocol-constrained type with postfix '?' is ambiguous and will be rejected in future version of Swift}} {{12-12=(}} {{19-19=)}}
  // expected-error(SWIFT4) @-2 {{non-protocol, non-class type 'P2?' cannot be used within a protocol-constrained type}}
  return x // expected-error(SWIFT4) {{cannot convert return expression}}
}

// Non-ident type at non-last position are rejected anyway.
typealias A1 = P1.Type & P2 // expected-error {{non-protocol, non-class type 'P1.Type' cannot be used within a protocol-constrained type}}
