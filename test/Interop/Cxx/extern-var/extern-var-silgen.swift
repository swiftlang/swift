// Test that global variables are handled properly by the ClangImporter.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -emit-sil -o %t/extern-var.sil -Xfrontend -enable-cxx-interop
// RUN: %FileCheck < %t/extern-var.sil %s

// CHECK: sil_global @counter : $Int32

// CHECK: sil hidden @$s4main10getCounters5Int32VyF : $@convention(thin) () -> Int32
// CHECK: %0 = global_addr @counter : $*Int32
// CHECK: begin_access [read] [dynamic] %0 : $*Int32

// CHECK: sil hidden @$s4main10setCounteryys5Int32VF : $@convention(thin) (Int32) -> ()
// CHECK: %1 = global_addr @counter : $*Int32
// CHECK: %3 = begin_access [modify] [dynamic] %1 : $*Int32

// sil hidden @$s4main20getNamespacedCounters5Int32VyF : $@convention(thin) () -> Int32
//FIXME mangle non-top-level var names to prevent name collisions
// %0 = global_addr @Namespaced.counter : $*Int32
// CHECK: begin_access [read] [dynamic] %0 : $*Int32

// CHECK: sil hidden @$s4main20setNamespacedCounteryys5Int32VF : $@convention(thin) (Int32) -> ()
//FIXME mangle non-top-level var names to prevent name collisions
// %1 = global_addr @Namespaced.counter : $*Int32
// CHECK: %4 = begin_access [modify] [dynamic] %1 : $*Int32

import ExternVar

func getCounter() -> CInt {
  return counter
}

func setCounter(_ c: CInt) {
  counter = c
}

func getNamespacedCounter() -> CInt {
  return Namespaced.counter
}

func setNamespacedCounter(_ c: CInt) {
  Namespaced.counter = c
}
