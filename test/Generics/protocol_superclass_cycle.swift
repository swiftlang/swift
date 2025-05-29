// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

protocol P : C {}
class C : P {}

// CHECK-LABEL: .SubP@
// CHECK-NEXT: Requirement signature: <Self where Self : P>
protocol SubP : P {}

protocol Q {
  associatedtype T
}

// CHECK-LABEL: .foo1@
// CHECK-NEXT: Generic signature: <T where T : P>
func foo1<T : P>(_: T) {}

// CHECK-LABEL: .foo1a@
// CHECK-NEXT: Generic signature: <T where T : P>
func foo1a<T>(_: T) where T : P, T : C {}

// CHECK-LABEL: .foo1b@
// CHECK-NEXT: Generic signature: <T where T : P>
func foo1b<T>(_: T) where T : C, T : P {}

// CHECK-LABEL: .foo2@
// CHECK-NEXT: Generic signature: <T where T : Q, T.[Q]T : P>
func foo2<T : Q>(_: T) where T.T : P {}

// CHECK-LABEL: .foo3@
// CHECK-NEXT: Generic signature: <T where T : SubP>
func foo3<T : SubP>(_: T) {}

// CHECK-LABEL: .foo4@
// CHECK-NEXT: Generic signature: <T where T : Q, T.[Q]T : SubP>
func foo4<T : Q>(_: T) where T.T : SubP {}

protocol SuperP {}

// CHECK-LABEL: .SubSuperP@
// CHECK-NEXT: Requirement signature: <Self where Self : SuperC>
protocol SubSuperP : SuperP, SuperC {}

class SuperC : SubSuperP {}

// CHECK-LABEL: .foo5@
// CHECK-NEXT: Generic signature: <T where T : SubSuperP>
func foo5<T : SubSuperP>(_: T) {}

// CHECK-LABEL: .foo6@
// CHECK-NEXT: Generic signature: <T where T : Q, T.[Q]T : SubSuperP>
func foo6<T : Q>(_: T) where T.T : SubSuperP {}

protocol POther where Self : COther {}
class COther : POther {}

// CHECK-LABEL: .foo7@
// CHECK-NEXT: Generic signature: <T where T : POther>
func foo7<T>(_: T) where T : COther, T : POther {}