// RUN: %target-swift-frontend %s -S -g -o - | FileCheck %s

// XFAIL: linux

func markUsed<T>(t: T) {}
var a = 1
var b = 2
markUsed(a+b)
// CHECK: _main:
// Verify that the top-level function (main) begins at line 6 (and not at line 0).
// CHECK: .loc    {{[0-9]}} 6 {{[0-9]}} prologue_end
