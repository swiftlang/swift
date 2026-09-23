// RUN: %target-swift-emit-silgen %clang-importer-sdk %s -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple | %FileCheck %s

// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: VENDOR=apple

import ctypes

// If a struct or union has C arrays but its memberwise init doesn't have a
// legacy projection, we still need an @available on the modern projection.
// (@available is only attached when targeting an Apple OS.)

// CHECK-LABEL: sil shared [transparent] [serialized] [available 26.0.0] [ossa] @$sSo28StructWithAllHugeArrayFieldsV{{[_0-9a-zA-Z$]*}}fC
func testAllHugeArrayFields(_ huge1: InlineArray<5000, Int32>, _ huge2: InlineArray<6000, Int32>) -> StructWithAllHugeArrayFields {
  return StructWithAllHugeArrayFields(huge1: huge1, huge2: huge2)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [available 26.0.0] [ossa] @$sSo32UnionWithSmallAndHugeArrayFieldsV{{[_0-9a-zA-Z$]*}}fC
func testHugeUnionField(_ v: InlineArray<5000, Int32>) -> UnionWithSmallAndHugeArrayFields {
  return UnionWithSmallAndHugeArrayFields(huge: v)
}
