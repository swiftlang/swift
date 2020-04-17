// Test that global variables are handled properly by the ClangImporter.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -emit-sil -o %t/extern-var.sil -Xfrontend -enable-cxx-interop
// RUN: %FileCheck < %t/extern-var.sil %s

// CHECK: %0 = global_addr @counter : $*Int32
//FIXME mangle non-top-level var names to prevent name collisions
// and check %0 = global_addr @_ZN10Namespaced7counterE : $*Int32

import ExternVar

func getCounter() -> CInt {
  return counter
}

func getNamespacedCounter() -> CInt {
  return Namespaced.counter
}
