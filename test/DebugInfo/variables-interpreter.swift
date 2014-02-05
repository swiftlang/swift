// Run test variables.swift, verifying that the combination of -i and -g works.
// RUN: %swift -i -g %p/variables.swift | FileCheck %s
// CHECK: 8, 16, 32
// REQUIRES: swift_interpreter
