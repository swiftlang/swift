// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/rdar79564324_other.swift -emit-module-path %t/rdar79564324_other.swiftmodule -dump-requirement-machine 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -I %t

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

// CHECK-LABEL: Requirement machine for fresh signature < T U >
// CHECK-NEXT: Rewrite system: {
// CHECK-NEXT: - [P].[P] => [P] [permanent]
// CHECK-NEXT: - [P].A => [P:A] [permanent]
// CHECK-NEXT: - [P:A].[P] => [P:A]
// CHECK-NEXT: - [P].[Copyable] => [P] [explicit]
// CHECK-NEXT: - [P].[Escapable] => [P] [explicit]
// CHECK-NEXT: - [P:A].[Copyable] => [P:A] [explicit]
// CHECK-NEXT: - [P:A].[Escapable] => [P:A] [explicit]
// CHECK-NEXT: - [P].[P:A] => [P:A]
// CHECK-NEXT: - [P:A].A => [P:A].[P:A]
// CHECK-NEXT: - [Copyable].[Copyable] => [Copyable] [permanent]
// CHECK-NEXT: - [Escapable].[Escapable] => [Escapable] [permanent]
// CHECK-NEXT: - τ_0_0.A => τ_0_0
// CHECK-NEXT: - τ_0_1.[P] => τ_0_1
// CHECK-NEXT: - τ_0_1.A => τ_0_0
// CHECK-NEXT: - τ_0_0.[Copyable] => τ_0_0 [explicit]
// CHECK-NEXT: - τ_0_0.[Escapable] => τ_0_0 [explicit]
// CHECK-NEXT: - τ_0_1.[Copyable] => τ_0_1 [explicit]
// CHECK-NEXT: - τ_0_1.[Escapable] => τ_0_1 [explicit]
// CHECK-NEXT: - τ_0_1.[P:A] => τ_0_0
// CHECK-NEXT: - τ_0_0.[P] => τ_0_0
// CHECK-NEXT: - τ_0_0.[P:A] => τ_0_0
// CHECK-NEXT: }
// CHECK: Property map: {
// CHECK-NEXT:   [P] => { conforms_to: [P Copyable Escapable] }
// CHECK-NEXT:   [P:A] => { conforms_to: [P Copyable Escapable] }
// CHECK-NEXT:   [Copyable] => { conforms_to: [Copyable] }
// CHECK-NEXT:   [Escapable] => { conforms_to: [Escapable] }
// CHECK-NEXT:   τ_0_1 => { conforms_to: [P Copyable Escapable] }
// CHECK-NEXT:   τ_0_0 => { conforms_to: [Copyable Escapable P] }
// CHECK-NEXT: }
