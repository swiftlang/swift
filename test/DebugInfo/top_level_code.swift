// RUN: %target-swift-frontend %s -S -g -o - | %FileCheck %s

// XFAIL: linux

func markUsed<T>(_ t: T) {}
// CHECK: _main:
// CHECK-NEXT: Lfunc_begin0:
// Verify that the top-level function (main) begins at line 0 and then
// proceeds to the first line.
// CHECK: .loc    {{[0-9]}} 0 {{[0-9]}} 
// CHECK-NOT: Lfunc_end0:
// CHECK: .loc    {{[0-9]}} [[@LINE+1]] {{[0-9]}} prologue_end
var a = 1
var b = 2
markUsed(a+b)
