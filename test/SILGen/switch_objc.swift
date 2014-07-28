// RUN: rm -rf %t.mcp
// RUN: %swift %clang-importer-sdk -emit-silgen %s -module-cache-path %t.mcp | FileCheck %s

import Foundation

// CHECK-LABEL: sil @_TF11switch_objc13matchesEitherFT5inputCSo4Hive1aS0_1bS0__Sb :
func matchesEither(#input: Hive, #a: Hive, #b: Hive) -> Bool {
  switch input {
  // CHECK:   function_ref @_TF10ObjectiveCoi2teFTCSo8NSObjectS0__Sb
  // CHECK:   cond_br {{%.*}}, [[YES_CASE1:bb[0-9]+]], [[NOT_CASE1:bb[0-9]+]]
  // CHECK: [[YES_CASE1]]:
  // CHECK:   br [[RET_TRUE:bb[0-9]+]]
  // CHECK: [[NOT_CASE1]]:
  // CHECK:   function_ref @_TF10ObjectiveCoi2teFTCSo8NSObjectS0__Sb
  // CHECK:   cond_br {{%.*}}, [[YES_CASE2:bb[0-9]+]], [[NOT_CASE2:bb[0-9]+]]
  // CHECK: [[YES_CASE2]]:
  // CHECK:   br [[RET_TRUE]]
  case a, b:
  // CHECK: [[RET_TRUE]]:
  // CHECK:   function_ref @_TFSb33_convertFromBuiltinBooleanLiteral
    return true

  // CHECK: [[NOT_CASE2]]:
  // CHECK:   br [[RET_FALSE:bb[0-9]+]]
  default:
  // CHECK: [[RET_FALSE]]:
  // CHECK:   function_ref @_TFSb33_convertFromBuiltinBooleanLiteral
    return false
  }
}

// CHECK-LABEL: sil @_TF11switch_objc9bridgedIs
// CHECK: bb0([[OBJ:%[0-9]+]] : $AnyObject):
func bridgedIs(obj: AnyObject) {
  // CHECK: function_ref @_TFSS21_bridgeFromObjectiveCfMSSFCSo8NSStringSS
  // CHECK: checked_cast_br [[OBJ]] : $AnyObject to $NSString
  obj as? String

  // CHECK: [[ARRAY_BRIDGE_FN:%[0-9]+]] = function_ref @_TFSa21_bridgeFromObjectiveCU__fMGSaQ__FCSo7NSArrayGSaQ__
  // CHECK: apply [[ARRAY_BRIDGE_FN]]<NSString>
  obj as? [NSString]
}
