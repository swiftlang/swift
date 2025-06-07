// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/Usage.swift %t/Conformance.swift

//--- Conformance.swift
protocol P {
  associatedtype A: Equatable
  func f() -> A
}

extension P {
  func f() -> A { fatalError() }
}

struct S: P {}

//--- Usage.swift
func callee(_: some Equatable) {}

// FIXME: This source location will become more accurate once TypeCheckExpr
// is a request! For now, pointing at the function is still better than crashing.

func caller() { // expected-error {{type 'S' does not conform to protocol 'P'}}
  callee(S().f())
}
