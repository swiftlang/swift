// RUN: %swift -sil-i %s | FileCheck %s
// CHECK: Nice shoes
func hello() {
  println("Nice shoes")
}

hello()
