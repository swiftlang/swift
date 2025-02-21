// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// CHECK: Nice shoes
func hello() {
  print("Nice shoes")
}

hello()
// UNSUPPORTED: OS=windows-msvc
