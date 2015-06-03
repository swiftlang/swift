// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://16168414 big int interpolation isn't working on 32-bit
// XFAIL: PTRSIZE=32

var hello = "Hello"
var pi = 3
// CHECK: Hello, world. Pi is approximately 3.
print("\(hello), world. Pi is approximately \(pi).")

// CHECK: Nested "Hello, World!" with Pi = 3!
var HW = "\(hello), World!";
print("Nested \"\(HW)\" with Pi = \(pi)!")


// CHECK: value = 1099226349619
var someval = 0xFFeeFF0033
print("value = \(someval)")


