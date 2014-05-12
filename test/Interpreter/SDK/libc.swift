// RUN: %target-run-simple-swift | FileCheck %s

import Darwin

// CHECK: Hello world
puts("Hello world")

// CHECK: 4294967295
println("\(UINT32_MAX)")
