// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

// Test SIL generation for imported default arguments.

// REQUIRES: objc_interop

import gizmo

// CHECK-LABEL: sil hidden @_TF26default_arguments_imported9testGizmo
func testGizmo(gizmo: Gizmo) {
  // CHECK: class_method [volatile] [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.enumerateSubGizmos!1.foreign
  // CHECK: function_ref @_TFSqCfT10nilLiteralT__GSqx_
  gizmo.enumerateSubGizmos()
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF26default_arguments_imported21testNonnullDictionaryFT5gizmoCSo5Gizmo_T_
func testNonnullDictionary(gizmo: Gizmo) {
  // CHECK: class_method [volatile] [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.doTheThing!1.foreign
  // CHECK-NOT: nilLiteral
  // CHECK: function_ref @_TFVs10DictionaryCft17dictionaryLiteralGSaTxq____GS_xq__
  gizmo.doTheThing()
} // CHECK: {{^}$}}

// CHECK-LABEL: sil hidden @_TF26default_arguments_imported22testNullableDictionaryFT5gizmoCSo5Gizmo_T_
func testNullableDictionary(gizmo: Gizmo) {
  // CHECK: class_method [volatile] [[SELF:%[0-9]+]] : $Gizmo, #Gizmo.doTheOtherThing!1.foreign
  // CHECK-NOT: dictionaryLiteral
  // CHECK: function_ref @_TFSqCfT10nilLiteralT__GSqx_
  gizmo.doTheOtherThing()
} // CHECK: {{^}$}}