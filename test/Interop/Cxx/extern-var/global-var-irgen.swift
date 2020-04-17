// Test that global variables are handled properly by the ClangImporter.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -emit-ir -o %t/global-var.ir -Xfrontend -enable-cxx-interop
// RUN: %FileCheck < %t/global-var.ir %s

// CHECK: @counter = external global i32, align 4
// CHECK: @_ZN10Namespaced7counterE = external global i32, align 4

import GlobalVar

func getCounter() -> CInt {
  return counter
}

func getNamespacedCounter() -> CInt {
  return Namespaced.counter
}
