// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-sil-ownership | %FileCheck %s

// Test SIL generation for imported default arguments.

// REQUIRES: objc_interop

import gizmo

// CHECK-LABEL: sil hidden @$s26default_arguments_imported9testGizmo{{[_0-9a-zA-Z]*}}F
func testGizmo(gizmo: Gizmo) {
  // CHECK: enum $Optional<@callee_guaranteed (@guaranteed Optional<Gizmo>) -> ()>, #Optional.none!enumelt
  // CHECK: objc_method [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.enumerateSubGizmos!1.foreign
  gizmo.enumerateSubGizmos()
} // CHECK: } // end sil function '$s26default_arguments_imported9testGizmo5gizmoySo0E0C_tF'

// CHECK-LABEL: sil hidden @$s26default_arguments_imported21testNonnullDictionary5gizmoySo5GizmoC_tF
func testNonnullDictionary(gizmo: Gizmo) {
  // CHECK-NOT: nilLiteral
  // CHECK: function_ref @$sSD17dictionaryLiteralSDyxq_Gx_q_td_tcfC
  // CHECK: objc_method [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.doTheThing!1.foreign
  gizmo.doTheThing()
} // CHECK: } // end sil function '$s26default_arguments_imported21testNonnullDictionary5gizmoySo5GizmoC_tF'

// CHECK-LABEL: sil hidden @$s26default_arguments_imported22testNullableDictionary5gizmoySo5GizmoC_tF
func testNullableDictionary(gizmo: Gizmo) {
  // CHECK-NOT: dictionaryLiteral
  // CHECK: enum $Optional<Dictionary<AnyHashable, Any>>, #Optional.none!enumelt
  // CHECK: objc_method [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.doTheOtherThing!1.foreign
  gizmo.doTheOtherThing()
} // CHECK: } // end sil function '$s26default_arguments_imported22testNullableDictionary5gizmoySo5GizmoC_tF'
