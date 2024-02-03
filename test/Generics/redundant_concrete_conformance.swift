// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {}
protocol P2 : P1 {}

protocol P3 {
  associatedtype A where A == S
}

struct S : P2 {}

func f1<T : P3>(_: T) where T.A : P1 {}
// CHECK-LABEL: .f1@
// CHECK-NEXT: Generic signature: <T where T : P3>

func f2<T : P3>(_: T) where T.A : P2 {}
// CHECK-LABEL: .f2@
// CHECK-NEXT: Generic signature: <T where T : P3>