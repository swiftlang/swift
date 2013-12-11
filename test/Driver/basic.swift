// RUN: %swift_driver -c %s -o %t 2>&1 | FileCheck %s

// CHECK-NOT: error

println("Hello, World!")
