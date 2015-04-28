// RUN: %target-swift-frontend -disable-func-sig-opts -O -sil-inline-threshold 0 -emit-sil -primary-file %s %S/Inputs/specialize_multifile_2.swift | FileCheck %s

// CHECK-LABEL: sil hidden @_TF20specialize_multifile12readFromDict
// Note: not specialized because Color : Hashable/Color : Equatable
// conformance is incomplete. When rdar://problem/20735544 is fixed, this
// should get specialized.
func readFromDict(color: Color, dict: [Color : String]) -> String {
  // CHECK-NOT:  return
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFVSs10Dictionaryg9subscriptFQ_GSqQ0__
  // CHECK-NOT: return
  // CHECK: apply [[FN]]<Color, String>
  return dict[color]!
}
