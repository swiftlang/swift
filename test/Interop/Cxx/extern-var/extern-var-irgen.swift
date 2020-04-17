// Test that global variables are handled properly by the ClangImporter.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -emit-ir -o %t/extern-var.ir -Xfrontend -enable-cxx-interop
// RUN: %FileCheck < %t/extern-var.ir %s

// CHECK: @counter = external global i32, align 4
// CHECK: @_ZN10Namespaced7counterE = external global i32, align 4

import ExternVar

func getCounter() -> CInt {
  return counter
}

func getNamespacedCounter() -> CInt {
  return Namespaced.counter
}
