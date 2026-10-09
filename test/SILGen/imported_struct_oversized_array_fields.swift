// RUN: %target-swift-emit-silgen %clang-importer-sdk %s -target %target-has-inline-array-triple > %t.sil
// RUN: %FileCheck %s --check-prefix=LEGACY --input-file %t.sil

// RUN: %target-swift-emit-silgen %clang-importer-sdk %s -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple > %t.sil
// RUN: %FileCheck %s --check-prefix=MODERN --input-file %t.sil

// REQUIRES: swift_feature_ModernImportedCArrays

import ctypes

#if hasFeature(ModernImportedCArrays)

// If a struct or union has C arrays but its memberwise init doesn't have a
// legacy projection, we still need an @available on the modern projection.

// MODERN-LABEL: sil shared [transparent] [serialized] [available 26.0.0] [ossa] @$sSo28StructWithAllHugeArrayFieldsV{{[_0-9a-zA-Z$]*}}fC
func testAllHugeArrayFields(_ huge1: InlineArray<5000, Int32>, _ huge2: InlineArray<6000, Int32>) -> StructWithAllHugeArrayFields {
  return StructWithAllHugeArrayFields(huge1: huge1, huge2: huge2)
}

// MODERN-LABEL: sil shared [transparent] [serialized] [available 26.0.0] [ossa] @$sSo32UnionWithSmallAndHugeArrayFieldsV{{[_0-9a-zA-Z$]*}}fC
func testHugeUnionField(_ v: InlineArray<5000, Int32>) -> UnionWithSmallAndHugeArrayFields {
  return UnionWithSmallAndHugeArrayFields(huge: v)
}

#else

// Make sure SILGen can emit bitcasts to InlineArray-of-InlineArrays without crashing.

// LEGACY-LABEL: sil shared [transparent] [serialized] [ossa] @$sSo26StructWithNestedArrayFieldV{{[_0-9a-zA-Z$]*}}fC
func testNestedArrayField(_ elems: ((Int32, Int32), (Int32, Int32))) -> StructWithNestedArrayField {
  return StructWithNestedArrayField(elems: elems)
}

// Make sure SILGen can emit bitcasts to arrays in address-only structs (here
// forced by a very large sibling field) without crashing. `huge` has no
// legacy form, so the legacy initializer omits it (leaving it zeroed)
// instead of exposing its modern (InlineArray) type.

// LEGACY-LABEL: sil shared [transparent] [serialized] [ossa] @$sSo33StructWithSmallAndHugeArrayFieldsV{{[_0-9a-zA-Z$]*}}fC
// LEGACY: builtin "zeroInitializer"
func testMixedArraySizes(_ small: (Int32, Int32, Int32, Int32)) -> StructWithSmallAndHugeArrayFields {
  return StructWithSmallAndHugeArrayFields(small: small)
}

#endif
