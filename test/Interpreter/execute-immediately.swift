// RUN: %swift_driver -e 'print("first")' -e 'print("second")' | %FileCheck %s

// REQUIRES: swift_interpreter

// CHECK: first
// CHECK: second
