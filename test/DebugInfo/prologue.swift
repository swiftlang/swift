// RUN: %swift -target x86_64-apple-macosx10.9 -primary-file %s -S -g -o - | FileCheck %s
// XFAIL: linux
// CHECK: .file [[F:[0-9]+]] "prologue.swift"
func bar<T, U>(x: T, y: U) { println("bar") }
// CHECK: .loc	[[F]] [[@LINE-1]] 3{{.}} prologue_end
// Make sure there is no allocation happening between the end of
// prologue and the beginning of the function body.
// CHECK-NOT: callq	*
// CHECK: callq	{{.*}}builtinStringLiteral
