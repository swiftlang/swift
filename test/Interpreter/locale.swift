// RUN: %target-run-simple-swift -sdk %sdk %s | FileCheck %s
// REQUIRES: sdk

import Darwin

// Ensure that printing of Double's is locale-insensitive.
// CHECK: x = 123.4
// CHECK: y = 42.0
setlocale(LC_ALL, "ru_RU.UTF-8")
let x = 123.4
let y = 42.0
println("x = \(x)")
println("y = \(y)")
