// RUN: %target-run-simple-swift | FileCheck %s

// CHECK: Nice shoes
func hello() {
  println("Nice shoes")
}

hello()
