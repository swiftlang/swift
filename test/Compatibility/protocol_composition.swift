// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -typecheck -primary-file %t/swift4.swift %t/common.swift -verify -swift-version 4

// BEGIN common.swift
protocol P1 {
  static func p1() -> Int
}
protocol P2 {
  static var p2: Int { get }
}

// BEGIN swift3.swift

// Warning for mistakenly accepted protocol composition production.
func foo(x: P1 & Any & P2.Type?) {
  // expected-warning @-1 {{protocol-constrained type with postfix '.Type' is ambiguous and will be rejected in future version of Swift}} {{13-13=(}} {{26-26=)}}
  let _: (P1 & P2).Type? = x
  let _: (P1 & P2).Type = x!
  let _: Int = x!.p1()
  let _: Int? = x?.p2
}

// Type expression
func bar() -> ((P1 & P2)?).Type {
  let x = (P1 & P2?).self
  // expected-warning @-1 {{protocol-constrained type with postfix '?' is ambiguous and will be rejected in future version of Swift}} {{12-12=(}} {{19-19=)}}
  return x
}

// Non-ident type at non-last position are rejected anyway.
typealias A1 = P1.Type & P2 // expected-error {{non-protocol, non-class type 'P1.Type' cannot be used within a protocol-constrained type}}

// BEGIN swift4.swift

func foo(x: P1 & Any & P2.Type?) { // expected-error {{non-protocol, non-class type 'P2.Type?' cannot be used within a protocol-constrained type}}
  let _: (P1 & P2).Type? = x // expected-error {{cannot convert value of type 'P1' to specified type '(P1 & P2).Type?'}}
  let _: (P1 & P2).Type = x! // expected-error {{cannot force unwrap value of non-optional type 'P1'}}
  let _: Int = x!.p1() // expected-error {{cannot force unwrap value of non-optional type 'P1'}}
  let _: Int? = x?.p2 // expected-error {{cannot use optional chaining on non-optional value of type 'P1'}}
}

func bar() -> ((P1 & P2)?).Type {
  let x = (P1 & P2?).self // expected-error {{non-protocol, non-class type 'P2?' cannot be used within a protocol-constrained type}}
  return x // expected-error {{cannot convert return expression}}
}

typealias A1 = P1.Type & P2 // expected-error {{non-protocol, non-class type 'P1.Type' cannot be used within a protocol-constrained type}}
