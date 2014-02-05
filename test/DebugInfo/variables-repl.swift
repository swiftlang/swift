// Run test variables.swift, verifying that the combination of repl and -g works
// RUN: %swift -g < %p/variables.swift | FileCheck %s
// CHECK: 8, 16, 32
// REQUIRES: swift_repl

// This test exposes heap overrun under ASan. Disable until the REPL code is fixed.
// XFAIL: asan
