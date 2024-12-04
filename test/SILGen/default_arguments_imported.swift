// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// Test SIL generation for imported default arguments.

// REQUIRES: objc_interop

import gizmo

// CHECK-LABEL: sil hidden [ossa] @$s26default_arguments_imported9testGizmo{{[_0-9a-zA-Z]*}}F
func testGizmo(gizmo: Gizmo) {
  // CHECK: enum $Optional<@callee_guaranteed (@guaranteed Optional<Gizmo>) -> ()>, #Optional.none!enumelt
  // CHECK: objc_method [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.enumerateSubGizmos!foreign
  gizmo.enumerateSubGizmos()
} // CHECK: } // end sil function '$s26default_arguments_imported9testGizmo5gizmoySo0E0C_tF'

// CHECK-LABEL: sil hidden [ossa] @$s26default_arguments_imported21testNonnullDictionary5gizmoySo5GizmoC_tF
func testNonnullDictionary(gizmo: Gizmo) {
  // CHECK-NOT: nilLiteral
  // CHECK: function_ref @$sSD17dictionaryLiteralSDyxq_Gx_q_td_tcfC
  // CHECK: objc_method [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.doTheThing!foreign
  gizmo.doTheThing()
} // CHECK: } // end sil function '$s26default_arguments_imported21testNonnullDictionary5gizmoySo5GizmoC_tF'

// CHECK-LABEL: sil hidden [ossa] @$s26default_arguments_imported22testNullableDictionary5gizmoySo5GizmoC_tF
func testNullableDictionary(gizmo: Gizmo) {
  // CHECK-NOT: dictionaryLiteral
  // CHECK: enum $Optional<Dictionary<AnyHashable, Any>>, #Optional.none!enumelt
  // CHECK: objc_method [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.doTheOtherThing!foreign
  gizmo.doTheOtherThing()
} // CHECK: } // end sil function '$s26default_arguments_imported22testNullableDictionary5gizmoySo5GizmoC_tF'
