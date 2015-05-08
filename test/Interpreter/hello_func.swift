// RUN: %target-run-simple-swift | FileCheck %s

// CHECK: Nice shoes
func hello() {
  print("Nice shoes")
}

hello()
