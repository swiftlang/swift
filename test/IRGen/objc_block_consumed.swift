
// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -Xllvm -sil-print-types -emit-silgen -disable-objc-attr-requires-foundation-module | %FileCheck %s

// We want to test that IRGen doesn't assert on this code. SIL is the best place
// to file check that the block parameter is actually +1.

// REQUIRES: PTRSIZE=64
// REQUIRES: objc_interop

import gizmo

// CHECK-LABEL: sil hidden [ossa] @$s19objc_block_consumed24passBlockWithConsumedArgyySo5GizmoC_ADtF : $@convention(thin) (@guaranteed Gizmo, @guaranteed Gizmo) -> () {
func passBlockWithConsumedArg(_ g: Gizmo, _ other: Gizmo) {
  // CHECK: objc_method %0 : $Gizmo, #Gizmo.perform!foreign : (Gizmo) -> (((Gizmo?) -> ())?) -> (), $@convention(objc_method) (Optional<@convention(block) (@owned Optional<Gizmo>) -> ()>, Gizmo) -> ()
  g.perform { other in }
}
