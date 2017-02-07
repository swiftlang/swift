// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/weak_other.swiftmodule -module-name=weak_other %S/Inputs/weak_other.swift
// RUN: %target-swift-frontend -I %t -emit-silgen %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

import weak_other

// CHECK-LABEL: sil hidden @_TF21weak_multiple_modules11doSomethingFT2uiC10weak_other2UI_Sb : $@convention(thin) (@owned UI) -> Bool
func doSomething(ui: UI) -> Bool {
  // CHECK: ref_element_addr
  // CHECK-objc: load_unowned
  // CHECK-native: load [take]
  // CHECK-native: strong_retain_unowned
  // CHECK: open_existential_ref
  // CHECK: witness_method
  // CHECK: apply
  // CHECK: open_existential_ref
  // CHECK: function_ref
  // CHECK: apply
  // CHECK: return
  return ui.environment.router.flags.asBoolean()
}
