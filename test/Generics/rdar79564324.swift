// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/rdar79564324_other.swift -emit-module-path %t/rdar79564324_other.swiftmodule -requirement-machine=on -debug-requirement-machine 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -I %t -requirement-machine=on

import rdar79564324_other

public func test<T : P>(_ t: T) where T == T.A {
  foo(from: t, to: t)
}

// foo(from:to:) has minimal signature <T, U where T == T.A, U : P, T.A == U.A>.
//
// The GSB had trouble re-building after deserialization because of the two
// requirements 'T == T.A' and 'T.A == U.A'.
//
// What should happen is that these two imply that 'T == U.A', and 'U : P'
// implies the existence of 'U.A' and thus the conformance of T to P.
//
// Instead what happens is that 'T == T.A' and 'T.A == U.A' both get delayed
// because T does not yet have a nested type A, so it gets stuck.
//
// The rewrite system handles this correctly though:

// CHECK-LABEL: Requirement machine for <τ_0_0, τ_0_1 where τ_0_0 == τ_0_0.A, τ_0_1 : P, τ_0_0.A == τ_0_1.A>
// CHECK-NEXT: Rewrite system: {
// CHECK-NEXT: - [P].A => [P:A]
// CHECK-NEXT: - [P:A].[P] => [P:A]
// CHECK-NEXT: - τ_0_0.[P:A] => τ_0_0
// CHECK-NEXT: - τ_0_1.[P] => τ_0_1
// CHECK-NEXT: - τ_0_1.[P:A] => τ_0_0
// CHECK-NEXT: - [P:A].A => [P:A].[P:A]
// CHECK-NEXT: - τ_0_0.[P] => τ_0_0
// CHECK-NEXT: - τ_0_1.A => τ_0_0
// CHECK-NEXT: - τ_0_0.A => τ_0_0
// CHECK-NEXT: }
// CHECK-NEXT: Equivalence class map: {
// CHECK-NEXT:   [P:A] => { conforms_to: [P] }
// CHECK-NEXT:   τ_0_0 => { conforms_to: [P] }
// CHECK-NEXT:   τ_0_1 => { conforms_to: [P] }
// CHECK-NEXT: }