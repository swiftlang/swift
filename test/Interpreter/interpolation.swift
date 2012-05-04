// RUN: %swift %s -i | FileCheck %s

var hello = "Hello"
var pi = 3
// CHECK: Hello, world. Pi is approximately 3.
println("\(hello), world. Pi is approximately \(pi).")

// CHECK: Nested "Hello, World!" with Pi = 3!
var HW = "\(hello), World!";
println("Nested \"\(HW)\" with Pi = \(pi)!")

