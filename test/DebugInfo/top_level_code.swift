// RUN: %target-swift-frontend %s -S -g -o - | %FileCheck %s

// XFAIL: linux

func markUsed<T>(_ t: T) {}
var a = 1
var b = 2
markUsed(a+b)
// CHECK: _main:
// Verify that the top-level function (main) begins at line 0 and then
// proceeds to line 6.
// CHECK: .loc    {{[0-9]}} 0 {{[0-9]}} prologue_end
// CHECK-NOT: .loc
// CHECK: .loc    {{[0-9]}} 6 {{[0-9]}}
