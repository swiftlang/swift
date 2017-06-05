// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/struct_with_initializer.swift -module-name extensions_multifile | %FileCheck %s

// CHECK-LABEL: sil hidden @_T020extensions_multifile12HasInitValueVACSi1z_tcfC : $@convention(method) (Int, @thin HasInitValue.Type) -> @owned HasInitValue {
// CHECK: function_ref @_T020extensions_multifile12HasInitValueV1xSivfi : $@convention(thin) () -> Int

// CHECK-LABEL: sil hidden_external [transparent] @_T020extensions_multifile12HasInitValueV1xSivfi : $@convention(thin) () -> Int

extension HasInitValue {
  init(z: Int) {}
}
