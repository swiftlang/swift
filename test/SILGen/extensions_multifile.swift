// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/struct_with_initializer.swift -module-name extensions_multifile | tee /tmp/xxx | %FileCheck %s

// CHECK-LABEL: sil hidden @_TFV20extensions_multifile12HasInitValueCfT1xSi_S0_ : $@convention(method) (Int, @thin HasInitValue.Type) -> HasInitValue {
// CHECK: function_ref @_TIvV20extensions_multifile12HasInitValue1xSii : $@convention(thin) () -> Int

// CHECK-LABEL: sil hidden_external [transparent] @_TIvV20extensions_multifile12HasInitValue1xSii : $@convention(thin) () -> Int

extension HasInitValue {
  init(x: Int) {}
}
