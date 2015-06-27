// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | FileCheck %s

// REQUIRES: objc_interop

import gizmo

extension Gizmo {
  // CHECK-LABEL: sil hidden @_TFE24objc_init_ref_delegationCSo5GizmocfMS0_FT3intSi_S0_ : $@convention(method) (Int, @owned Gizmo) -> @owned Gizmo
  convenience init(int i: Int) {
    // CHECK: bb0([[I:%[0-9]+]] : $Int, [[ORIG_SELF:%[0-9]+]] : $Gizmo):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box $Gizmo
    // CHECK:   [[SELFMUI:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]#1 : $*Gizmo
    // CHECK:   store [[ORIG_SELF]] to [[SELFMUI]] : $*Gizmo
    // CHECK:   [[SELF:%[0-9]+]] = load [[SELFMUI]] : $*Gizmo
    // CHECK:   [[INIT_DELEG:%[0-9]+]] = class_method [volatile] [[SELF]] : $Gizmo, #Gizmo.init!initializer.1.foreign : Gizmo.Type -> (bellsOn: Int) -> Gizmo! , $@convention(objc_method) (Int, @owned Gizmo) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
    // CHECK:   [[SELF_RET:%[0-9]+]] = apply [[INIT_DELEG]]([[I]], [[SELF]]) : $@convention(objc_method) (Int, @owned Gizmo) -> @owned ImplicitlyUnwrappedOptional<Gizmo>
    // CHECK:   store [[SELF_RET]] to [[SELFMUI:%[0-9]+]]#1 : $*ImplicitlyUnwrappedOptional<Gizmo>
    // CHECK:   strong_retain [[SELF4:%[0-9]+]] : $Gizmo
    // CHECK:   strong_release [[SELF_BOX:%[0-9]+]]#0 : $@box Gizmo
    // CHECK:   return [[SELF4]] : $Gizmo
    self.init(bellsOn:i)
  }
}
