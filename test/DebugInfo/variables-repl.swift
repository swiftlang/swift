// Run test variables.swift, verifying that the combination of repl and -g works
// RUN: %swift -g -repl < %S/variables.swift | %FileCheck %s
// CHECK: 8, 16, 32
// REQUIRES: swift_repl
