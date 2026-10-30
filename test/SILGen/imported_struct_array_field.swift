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

// Test compatibility with anonymous unions (and indirect fields into them).

// LEGACY-LABEL: sil hidden [ossa] @$s{{.*}}readAnonymousUnionElement
// MODERN-LABEL: sil hidden [ossa] @$s{{.*}}readAnonymousUnionElement
// LEGACY-LABEL: sil hidden [ossa] @$s{{.*}}writeAnonymousUnionElement
// MODERN-LABEL: sil hidden [ossa] @$s{{.*}}writeAnonymousUnionElement
#if hasFeature(ModernImportedCArrays)
func readAnonymousUnionElement(_ s: StructWithAnonymousUnionArray) -> CChar {
  s.name[0]
}

func writeAnonymousUnionElement(_ s: inout StructWithAnonymousUnionArray) {
  s.name[1] = 3
}
#else
func readAnonymousUnionElement(_ s: StructWithAnonymousUnionArray) -> CChar {
  s.name.0
}

func writeAnonymousUnionElement(_ s: inout StructWithAnonymousUnionArray) {
  s.name.1 = 3
}
#endif

// A modern projection of a stored legacy field is accessed in place, by
// casting the address of the stored field.

// LEGACY-LABEL: sil hidden [ossa] @$s{{.*}}writeArrayField
// LEGACY-NOT: unchecked_addr_cast
// LEGACY: struct_element_addr {{%.*}}, #StructWithPlainAndArrayFields.small
// LEGACY: tuple_addr_constructor [assign]
// MODERN-LABEL: sil hidden [ossa] @$s{{.*}}writeArrayField
// MODERN: [[ACCESS:%.*]] = begin_access [modify] [unknown] %0
// MODERN: [[FIELD:%.*]] = struct_element_addr [[ACCESS]], #StructWithPlainAndArrayFields.small
// MODERN: [[CAST:%.*]] = unchecked_addr_cast [[FIELD]] to $*InlineArray<4, Int32>
// MODERN: assign {{%.*}} to [[CAST]]
// MODERN: end_access [[ACCESS]]
#if hasFeature(ModernImportedCArrays)
func writeArrayField(_ s: inout StructWithPlainAndArrayFields,
                     _ a: InlineArray<4, CInt>) {
  s.small = a
}
#else
func writeArrayField(_ s: inout StructWithPlainAndArrayFields,
                     _ a: (CInt, CInt, CInt, CInt)) {
  s.small = a
}
#endif
