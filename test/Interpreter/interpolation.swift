// RUN: %target-run-simple-swift | FileCheck %s

// FIXME: rdar://16168414 big int interpolation isn't working on 32-bit
// XFAIL: PTRSIZE=32

var hello = "Hello"
var pi = 3
// CHECK: Hello, world. Pi is approximately 3.
println("\(hello), world. Pi is approximately \(pi).")

// CHECK: Nested "Hello, World!" with Pi = 3!
var HW = "\(hello), World!";
println("Nested \"\(HW)\" with Pi = \(pi)!")


// CHECK: value = 1099226349619 hex = 0xFFEEFF0033 oct = 0o17775677600063
var someval = 0xFFeeFF0033
println("value = \(someval) hex = 0x\(someval, radix:16, uppercase : true) oct = 0o\(someval, radix:8)")


