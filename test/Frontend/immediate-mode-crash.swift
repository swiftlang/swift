/// Don't ask to file a bug report on a script failure.

// REQUIRES: swift_interpreter

// RUN: not --crash %target-swift-frontend -interpret %s 2>&1 | %FileCheck %s

assert(false)
// CHECK: Assertion failed
// CHECK-NOT: Please submit a bug report
