// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | FileCheck %s

import gizmo

extension Gizmo {
  // CHECK-DAG: sil @_TFCSo5GizmocfMS_FT3intSi_S_ : $@cc(method) @thin (Int, @owned Gizmo) -> @owned Gizmo
  convenience init(int i: Int) {
    // CHECK: bb0([[I:%[0-9]+]] : $Int, [[ORIG_SELF:%[0-9]+]] : $Gizmo):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box $Gizmo
    // CHECK:   [[SELF:%[0-9]+]] = mark_uninitialized [delegatingself] [[ORIG_SELF]] : $Gizmo
    // CHECK:   store [[SELF]] to [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   [[SELF:%[0-9]+]] = load [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   [[INIT_DELEG:%[0-9]+]] = class_method [volatile] [[SELF]] : $Gizmo, #Gizmo.init!initializer.1.foreign : Gizmo.Type -> (bellsOn: Int) -> Gizmo , $@cc(objc_method) @thin (Int, @owned Gizmo) -> @owned Gizmo
    // CHECK:   [[SELF_RET:%[0-9]+]] = apply [[INIT_DELEG]]([[I]], [[SELF]]) : $@cc(objc_method) @thin (Int, @owned Gizmo) -> @owned Gizmo
    // CHECK:   store [[SELF_RET]] to [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   [[SELF4:%[0-9]+]] = load [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   strong_retain [[SELF4]] : $Gizmo
    // CHECK:   strong_release [[SELF_BOX]]#0 : $Builtin.NativeObject
    // CHECK:   return [[SELF4]] : $Gizmo
    self.init(bellsOn:i)
  }
}
