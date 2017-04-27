// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

extension Gizmo {
  // CHECK-LABEL: sil hidden @_T0So5GizmoC24objc_init_ref_delegationE{{[_0-9a-zA-Z]*}}fc
  convenience init(int i: Int) {
    // CHECK: bb0([[I:%[0-9]+]] : $Int, [[ORIG_SELF:%[0-9]+]] : $Gizmo):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Gizmo }
    // CHECK:   [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
    // CHECK:   store [[ORIG_SELF]] to [init] [[PB_BOX]] : $*Gizmo
    // CHECK:   [[SELF:%[0-9]+]] = load [take] [[PB_BOX]] : $*Gizmo
    // CHECK:   [[INIT_DELEG:%[0-9]+]] = class_method [volatile] [[SELF]] : $Gizmo, #Gizmo.init!initializer.1.foreign : (Gizmo.Type) -> (Int) -> Gizmo!, $@convention(objc_method) (Int, @owned Gizmo) -> @owned Optional<Gizmo>
    // CHECK:   [[SELF_RET:%[0-9]+]] = apply [[INIT_DELEG]]([[I]], [[SELF]]) : $@convention(objc_method) (Int, @owned Gizmo) -> @owned Optional<Gizmo>
    // CHECK:   [[SELF4:%.*]] = load [copy] [[PB_BOX]]
    // CHECK:   destroy_value [[MARKED_SELF_BOX]]
    // CHECK:   return [[SELF4]] : $Gizmo
    self.init(bellsOn:i)
  }
}
