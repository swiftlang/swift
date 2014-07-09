// Run test variables.swift, verifying that the combination of -interpret and -g works.
// RUN: %swift -g -interpret %S/variables.swift | FileCheck %s
// CHECK: 8, 16, 32
// REQUIRES: swift_interpreter
