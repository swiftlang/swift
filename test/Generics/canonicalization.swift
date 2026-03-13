// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// rdar://problem/23149063
protocol P0 { }

protocol P {
  associatedtype A
}

protocol Q : P {
  associatedtype A
}

// CHECK-LABEL: .f(t:)@
// CHECK-NEXT: Generic signature: <T where T : Q, T.[P]A : P0>
func f<T>(t: T) where T : P, T : Q, T.A : P0 { } // expected-note{{'f(t:)' previously declared here}}

// CHECK-LABEL: .f(t:)@
// CHECK-NEXT: Generic signature: <T where T : Q, T.[P]A : P0>
func f<T>(t: T) where T : Q, T : P, T.A : P0 { } // expected-error{{invalid redeclaration of 'f(t:)'}}

