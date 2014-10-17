// RUN: %swift %s -S -g -o - | FileCheck %s
var a = 1
var b = 2
println(a+b)
// CHECK: _main:
// Verify that the top-level function (main) begins at line 2 (and not at line 0).
// CHECK: .loc    {{[0-9]}} 2 {{[0-9]}} prologue_end
