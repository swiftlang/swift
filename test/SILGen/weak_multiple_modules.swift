// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/weak_other.swiftmodule -module-name=weak_other %S/Inputs/weak_other.swift
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -I %t -emit-silgen %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

import weak_other

// CHECK-LABEL: sil hidden @_T021weak_multiple_modules11doSomethingSb0A6_other2UIC2ui_tF : $@convention(thin) (@owned UI) -> Bool
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
