// RUN: %swift -triple x86_64-apple-darwin10 %s -S -g -o - | FileCheck %s
func bar <T,U> (x : T, y : U) { println("bar") }
// CHECK: .loc	1 [[@LINE-1]] 33 prologue_end
// Make sure there is no allocation happening between the end of
// prologue and the beginning of the function body.
// CHECK-NOT: callq	*
// CHECK: callq	{{.*}}convertFromBuiltinStringLiteral
