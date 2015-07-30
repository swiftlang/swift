// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

defer { print("deferred 1") }
defer { print("deferred 2") }
print("start!")

// CHECK-NOT: deferred
// CHECK-LABEL: start!
// CHECK-NEXT: deferred 2
// CHECK-NEXT: deferred 1
