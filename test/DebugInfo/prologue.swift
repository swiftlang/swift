// RUN: %target-swift-frontend -primary-file %s -S -g -o - | FileCheck %s

// REQUIRES: CPU=x86_64

// CHECK: .file [[F:[0-9]+]] "prologue.swift"
func bar<T, U>(x: T, y: U) { println("bar") }
// CHECK: .loc	[[F]] [[@LINE-1]] 3{{.}} prologue_end
// Make sure there is no allocation happening between the end of
// prologue and the beginning of the function body.
// CHECK-NOT: callq	*
// CHECK: callq	{{.*}}builtinStringLiteral
