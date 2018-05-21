
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/weak_other.swiftmodule -module-name=weak_other %S/Inputs/weak_other.swift
// RUN: %target-swift-emit-silgen -module-name weak_multiple_modules -I %t -enable-sil-ownership %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

import weak_other

// CHECK-LABEL: sil hidden @$S21weak_multiple_modules11doSomething2uiSb0A6_other2UIC_tF : $@convention(thin) (@guaranteed UI) -> Bool
func doSomething(ui: UI) -> Bool {
  // CHECK: ref_element_addr
  // CHECK-objc: load_unowned
  // CHECK-native: load_borrow
  // CHECK-native: copy_unowned_value
  // CHECK-native: end_borrow
  // CHECK: open_existential_ref
  // CHECK: witness_method
  // CHECK: apply
  // CHECK: open_existential_ref
  // CHECK: function_ref
  // CHECK: apply
  // CHECK: return
  return ui.environment.router.flags.asBoolean()
}
