// Test that global variables are handled properly by the ClangImporter.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -emit-sil -o %t/global-var.sil -Xfrontend -enable-cxx-interop
// RUN: %FileCheck < %t/global-var.sil %s

// CHECK: %0 = global_addr @counter : $*Int32
// CHECK: %0 = global_addr @_ZN10Namespaced7counterE : $*Int32

import GlobalVar

func getCounter() -> CInt {
  return counter
}

func getNamespacedCounter() -> CInt {
  return Namespaced.counter
}
