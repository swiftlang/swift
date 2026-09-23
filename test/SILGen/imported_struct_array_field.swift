// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -enable-objc-interop -disable-objc-attr-requires-foundation-module %clang-importer-sdk %s
// RUN: %target-swift-emit-silgen -enable-objc-interop -disable-objc-attr-requires-foundation-module %clang-importer-sdk %s | %FileCheck %s --check-prefix=LEGACY

// RUN: %target-swift-emit-silgen -enable-objc-interop -disable-objc-attr-requires-foundation-module %clang-importer-sdk %s -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple | %FileCheck %s --check-prefix=MODERN
// REQUIRES: swift_feature_ModernImportedCArrays

import ctypes

// LEGACY-LABEL: sil shared [transparent] [serialized] [ossa] @$sSo27StructWithArrayTypedefFieldV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Int32, Int32, Int32, Int32, @thin StructWithArrayTypedefField.Type) -> StructWithArrayTypedefField
// MODERN-LABEL: sil shared [transparent] [serialized]{{ \[available 26.0.0\] | }}[ossa] @$sSo27StructWithArrayTypedefFieldV{{[_0-9a-zA-Z$]*}}fC : $@convention(method) (InlineArray<4, Int32>, @thin StructWithArrayTypedefField.Type) -> StructWithArrayTypedefField
#if hasFeature(ModernImportedCArrays)
func useImportedArrayTypedefInit() -> StructWithArrayTypedefField {
  return StructWithArrayTypedefField(small: [0, 0, 0, 0])
}
#else
func useImportedArrayTypedefInit() -> StructWithArrayTypedefField {
  return StructWithArrayTypedefField(small: (0, 0, 0, 0))
}
#endif
