// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-sil-ownership -enable-objc-interop | %FileCheck %s

import gizmo

extension Gizmo {
  // CHECK-LABEL: sil hidden @$sSo5GizmoC24objc_init_ref_delegationE{{[_0-9a-zA-Z]*}}fC
  convenience init(int i: Int) {
    // CHECK: bb0([[I:%[0-9]+]] : $Int, [[SELF_META:%[0-9]+]] : $@thick Gizmo.Type):
    // CHECK:   [[SELF_BOX:%[0-9]+]] = alloc_box ${ var Gizmo }
    // CHECK:   [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
    // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
    // CHECK:   [[SELF_OBJC_META:%.*]] = thick_to_objc_metatype [[SELF_META]]
    // CHECK:   [[ORIG_SELF:%.*]] = alloc_ref_dynamic [objc] [[SELF_OBJC_META]]
    // CHECK:   [[INIT_DELEG:%[0-9]+]] = objc_method [[ORIG_SELF]] : $Gizmo, #Gizmo.init!initializer.1.foreign : (Gizmo.Type) -> (Int) -> Gizmo?, $@convention(objc_method) (Int, @owned Gizmo) -> @owned Optional<Gizmo>
    // CHECK:   [[SELF_RET:%[0-9]+]] = apply [[INIT_DELEG]]([[I]], [[ORIG_SELF]]) : $@convention(objc_method) (Int, @owned Gizmo) -> @owned Optional<Gizmo>
    // CHECK:   [[SELF4:%.*]] = load [copy] [[PB_BOX]]
    // CHECK:   destroy_value [[MARKED_SELF_BOX]]
    // CHECK:   return [[SELF4]] : $Gizmo
    self.init(bellsOn:i)
  }
}
