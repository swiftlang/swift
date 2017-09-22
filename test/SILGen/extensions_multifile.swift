// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/struct_with_initializer.swift -module-name extensions_multifile -enable-sil-ownership | %FileCheck %s

// CHECK-LABEL: sil hidden @_T020extensions_multifile12HasInitValueVACSi1z_tcfC : $@convention(method) (Int, @thin HasInitValue.Type) -> @owned HasInitValue {
// CHECK: function_ref @_T020extensions_multifile12HasInitValueV1xSivpfi : $@convention(thin) () -> Int

// CHECK-LABEL: sil hidden_external [transparent] @_T020extensions_multifile12HasInitValueV1xSivpfi : $@convention(thin) () -> Int

extension HasInitValue {
  init(z: Int) {}
}

// CHECK-LABEL: sil hidden_external [transparent] @_T020extensions_multifile19HasPrivateInitValueV1x33_0A683B047698EED319CF48214D7F519DLLSivpfi : $@convention(thin) () -> Int

extension HasPrivateInitValue {
  init(z: Int) {}
}

// CHECK-LABEL: sil hidden_external [transparent] @_T020extensions_multifile24PublicStructHasInitValueV1xSivpfi : $@convention(thin) () -> Int

extension PublicStructHasInitValue {
  init(z: Int) {}
}
