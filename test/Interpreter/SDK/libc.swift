// RUN: %target-run-simple-swift | FileCheck %s

import Darwin

// CHECK: Hello world
fputs("Hello world", stdout)

// CHECK: 4294967295
println("\(UINT32_MAX)")
