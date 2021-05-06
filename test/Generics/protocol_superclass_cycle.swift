// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

protocol P : C {}
final class C : P {}

// CHECK: Generic signature: <T where T : P>
func foo<T : P>(_: T) {}
