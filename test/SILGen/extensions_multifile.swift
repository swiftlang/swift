// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -primary-file %s %S/Inputs/struct_with_initializer.swift -module-name extensions_multifile | %FileCheck %s

// CHECK-LABEL: sil hidden @_T020extensions_multifile12HasInitValueVACSi1x_tcfC : $@convention(method) (Int, @thin HasInitValue.Type) -> HasInitValue {
// CHECK: function_ref @_T020extensions_multifile12HasInitValueV1xSivfi : $@convention(thin) () -> Int

// CHECK-LABEL: sil hidden_external [transparent] @_T020extensions_multifile12HasInitValueV1xSivfi : $@convention(thin) () -> Int

extension HasInitValue {
  init(x: Int) {}
}
