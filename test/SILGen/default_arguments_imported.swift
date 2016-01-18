// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen -enable-infer-default-arguments | FileCheck %s

// Test SIL generation for imported default arguments.

// REQUIRES: objc_interop

import gizmo

// CHECK-LABEL: sil hidden @_TF26default_arguments_imported9testGizmo
func testGizmo(gizmo: Gizmo) {
  // CHECK: class_method [volatile] [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.enumerateSubGizmos!1.foreign
  // CHECK-NOT: return
  // CHECK: function_ref @_TFSqCfT10nilLiteralT__GSqx_
  gizmo.enumerateSubGizmos()
}
