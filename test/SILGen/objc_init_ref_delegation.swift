// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -triple x86_64-apple-darwin13 -sdk=%S/Inputs %s -emit-silgen | FileCheck %s

import gizmo

extension Gizmo {
  // CHECK-DAG: sil @_TFCSo5GizmocfMS_FT7withIntSi_S_ : $@cc(method) @thin (Int64, @owned Gizmo) -> @owned Gizmo
  init withInt(i: NSInteger) {
    // CHECK: bb0([[I:%[0-9]+]] : $Int64, [[ORIG_SELF:%[0-9]+]] : $Gizmo):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box $Gizmo
    // CHECK:   [[I_BOX:%[0-9]+]] = alloc_box $Int64
    // CHECK:   store [[I]] to [[I_BOX]]#1 : $*Int64
    // CHECK:   [[SELF:%[0-9]+]] = mark_uninitialized [delegatingderivedself] [[ORIG_SELF]] : $Gizmo
    // CHECK:   store [[SELF]] to [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   [[SELF:%[0-9]+]] = load [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   strong_retain [[SELF]] : $Gizmo
    // CHECK:   [[INIT_DELEG:%[0-9]+]] = class_method [volatile] [[SELF]] : $Gizmo, #Gizmo.init!initializer.1.foreign : $@cc(objc_method) @thin (Int64, @owned Gizmo) -> @owned Gizmo
    // CHECK:   [[I:%[0-9]+]] = load [[I_BOX]]#1 : $*Int64
    // CHECK:   [[SELF_RET:%[0-9]+]] = apply [[INIT_DELEG]]([[I]], [[SELF]]) : $@cc(objc_method) @thin (Int64, @owned Gizmo) -> @owned Gizmo
    // CHECK:   assign [[SELF_RET]] to [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   strong_release [[I_BOX]]#0 : $Builtin.ObjectPointer
    // CHECK:   [[SELF4:%[0-9]+]] = load [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   strong_retain [[SELF4]] : $Gizmo
    // CHECK:   strong_release [[SELF_BOX]]#0 : $Builtin.ObjectPointer
    // CHECK:   return [[SELF4]] : $Gizmo
    self.init(withBellsOn:i)
  }
}
