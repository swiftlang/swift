// RUN: %target-swift-frontend -primary-file %s -S -g -o - | %FileCheck %s

// REQUIRES: CPU=x86_64

func markUsed<T>(_ t: T) {}

// CHECK: .file [[F:[0-9]+]] "{{.*}}prologue.swift"
func bar<T, U>(_ x: T, y: U) { markUsed("bar") }
// CHECK: _TF8prologue3baru0_rFTx1yq__T_:
// CHECK: .loc	[[F]] 0 0 prologue_end
// Make sure there is no allocation happening between the end of
// prologue and the beginning of the function body.
// CHECK-NOT: callq	*
// CHECK: .loc	[[F]] [[@LINE-6]] {{.}}
// CHECK: callq	{{.*}}builtinStringLiteral
