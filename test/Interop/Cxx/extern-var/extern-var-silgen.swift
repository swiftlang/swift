// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -emit-sil -o %t/extern-var.sil -Xfrontend -enable-cxx-interop
// RUN: %FileCheck < %t/extern-var.sil %s

import ExternVar

func getCounter() -> CInt {
  return counter
}

// CHECK: sil_global @counter : $Int32

// CHECK: sil hidden @$s4main10getCounters5Int32VyF : $@convention(thin) () -> Int32
// CHECK: [[COUNTER:%.*]] = global_addr @counter : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[COUNTER]] : $*Int32
// CHECK: [[LOAD:%.*]] = load [[ACCESS]] : $*Int32
// CHECK: return [[LOAD]] : $Int32

func setCounter(_ c: CInt) {
  counter = c
}

// CHECK: sil hidden @$s4main10setCounteryys5Int32VF : $@convention(thin) (Int32) -> ()
// CHECK: [[COUNTER:%.*]] = global_addr @counter : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[COUNTER]] : $*Int32
// CHECK: store %0 to [[ACCESS]] : $*Int32

func getNamespacedCounter() -> CInt {
  return Namespaced.counter
}

// sil hidden @$s4main20getNamespacedCounters5Int32VyF : $@convention(thin) () -> Int32
//FIXME mangle non-top-level var names to prevent name collisions
// %0 = global_addr @Namespaced.counter : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] %0 : $*Int32
// CHECK: [[LOAD:%.*]] = load [[ACCESS]] : $*Int32
// CHECK: return [[LOAD]] : $Int32

func setNamespacedCounter(_ c: CInt) {
  Namespaced.counter = c
}

// CHECK: sil hidden @$s4main20setNamespacedCounteryys5Int32VF : $@convention(thin) (Int32) -> ()
//FIXME mangle non-top-level var names to prevent name collisions
// %1 = global_addr @Namespaced.counter : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] %1 : $*Int32
// CHECK: store %0 to [[ACCESS]] : $*Int32

func modifyInout(_ c: inout CInt) {
  c = 42
}

func passingVarAsInout() {
  modifyInout(&counter)
}

// CHECK: sil hidden @$s4main17passingVarAsInoutyyF : $@convention(thin) () -> ()
// CHECK: [[COUNTER:%.*]] = global_addr @counter : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[COUNTER]] : $*Int32
// CHECK: [[FUNCTION:%.*]] = function_ref @$s4main11modifyInoutyys5Int32VzF : $@convention(thin) (@inout Int32) -> ()
// CHECK: apply [[FUNCTION]]([[ACCESS]]) : $@convention(thin) (@inout Int32) -> ()
