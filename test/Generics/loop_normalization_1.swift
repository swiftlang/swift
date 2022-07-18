// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P {
  associatedtype T
}

struct C {}

// CHECK-LABEL: .f1@
// CHECK-NEXT: Generic signature: <T, U where T : P, U : P, T.[P]T == C, U.[P]T == C>
func f1<T: P, U: P>(_: T, _: U) where T.T == C, T.T == U.T { }

// CHECK-LABEL: .f2@
// CHECK-NEXT: Generic signature: <T, U where T : P, U : P, T.[P]T == C, U.[P]T == C>
func f2<T: P, U: P>(_: T, _: U) where U.T == C, T.T == U.T { }

// CHECK-LABEL: .f3@
// CHECK-NEXT: Generic signature: <T, U where T : P, U : P, T.[P]T == C, U.[P]T == C>
func f3<T: P, U: P>(_: T, _: U) where T.T == C, U.T == C, T.T == U.T { }

// CHECK-LABEL: .f4@
// CHECK-NEXT: Generic signature: <T, U where T : P, U : P, T.[P]T == C, U.[P]T == C>
func f4<T: P, U: P>(_: T, _: U) where T.T == C, T.T == U.T, U.T == C { }

// CHECK-LABEL: .f5@
// CHECK-NEXT: Generic signature: <T, U where T : P, U : P, T.[P]T == C, U.[P]T == C>
func f5<T: P, U: P>(_: T, _: U) where T.T == U.T, T.T == C, U.T == C { }

// CHECK-LABEL: .f6@
// CHECK-NEXT: Generic signature: <T, U where T : P, U : P, T.[P]T == C, U.[P]T == C>
func f6<T: P, U: P>(_: T, _: U) where U.T == C, T.T == C, T.T == U.T { }

// CHECK-LABEL: .f7@
// CHECK-NEXT: Generic signature: <T, U where T : P, U : P, T.[P]T == C, U.[P]T == C>
func f7<T: P, U: P>(_: T, _: U) where T.T == C, U.T == C, T.T == U.T { }

// CHECK-LABEL: .f8@
// CHECK-NEXT: Generic signature: <T, U where T : P, U : P, T.[P]T == C, U.[P]T == C>
func f8<T: P, U: P>(_: T, _: U) where T.T == U.T, U.T == C, T.T == C { }
