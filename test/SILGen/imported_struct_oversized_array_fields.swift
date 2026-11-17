// RUN: %target-swift-emit-silgen %clang-importer-sdk %s -target %target-has-inline-array-triple > %t.sil
// RUN: %FileCheck %s --check-prefix=LEGACY --input-file %t.sil

// RUN: %target-swift-emit-silgen %clang-importer-sdk %s -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple > %t.sil
// RUN: %FileCheck %s --check-prefix=MODERN --input-file %t.sil

// RUN: %target-swift-emit-silgen %clang-importer-sdk %s -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -Xllvm -sil-print-types > %t.types.sil
// RUN: %FileCheck %s --check-prefix=TYPES --input-file %t.types.sil

// REQUIRES: swift_feature_ModernImportedCArrays

import ctypes

#if hasFeature(ModernImportedCArrays)

// If a struct or union has C arrays but its memberwise init doesn't have a
// legacy projection, the modern projection must still be emitted. (Its
// availability is checked in imported_struct_oversized_array_fields_availability.swift.)

// MODERN-LABEL: sil shared [transparent] [serialized] {{(\[available [0-9.]+\] )?}}[ossa] @$sSo28StructWithAllHugeArrayFieldsV{{[_0-9a-zA-Z$]*}}fC
func testAllHugeArrayFields(_ huge1: InlineArray<5000, Int32>, _ huge2: InlineArray<6000, Int32>) -> StructWithAllHugeArrayFields {
  return StructWithAllHugeArrayFields(huge1: huge1, huge2: huge2)
}

// MODERN-LABEL: sil shared [transparent] [serialized] {{(\[available [0-9.]+\] )?}}[ossa] @$sSo32UnionWithSmallAndHugeArrayFieldsV{{[_0-9a-zA-Z$]*}}fC
func testHugeUnionField(_ v: InlineArray<5000, Int32>) -> UnionWithSmallAndHugeArrayFields {
  return UnionWithSmallAndHugeArrayFields(huge: v)
}

// The hidden storage for a huge array of small arrays uses Builtin.FixedArray
// at every level, rather than a tuple for the inner arrays.

// TYPES-LABEL: sil hidden [ossa] @$s{{.*}}testHugeOfSmallStorage
// TYPES: [[FIELD:%.*]] = struct_element_addr {{%.*}} : $*StructWithHugeArrayOfSmallArrays, #StructWithHugeArrayOfSmallArrays.grid
// TYPES: unchecked_addr_cast [[FIELD]] : $*Builtin.FixedArray<5000, Builtin.FixedArray<2, Int32>> to $*InlineArray<5000, InlineArray<2, Int32>>
func testHugeOfSmallStorage(_ s: inout StructWithHugeArrayOfSmallArrays,
                            _ v: InlineArray<5000, InlineArray<2, Int32>>) {
  s.grid = v
}

// Globals and static members are stored the same way, and can be assigned
// through their forwarding setter.

// TYPES-LABEL: sil hidden [ossa] @$s{{.*}}testHugeGlobalAndStatic
// TYPES-DAG: [[GLOBAL:%.*]] = global_addr @hugeGlobalArray : $*Builtin.FixedArray<5000, Int32>
// TYPES-DAG: [[STATIC:%.*]] = global_addr @hugeStaticArray : $*Builtin.FixedArray<5000, Int32>
// TYPES-DAG: [[SMALL:%.*]] = global_addr @smallStaticArray : $*(Int32, Int32, Int32, Int32)
// TYPES: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[GLOBAL]]
// TYPES: unchecked_addr_cast [[ACCESS]] : $*Builtin.FixedArray<5000, Int32> to $*InlineArray<5000, Int32>
// TYPES: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[STATIC]]
// TYPES: unchecked_addr_cast [[ACCESS]] : $*Builtin.FixedArray<5000, Int32> to $*InlineArray<5000, Int32>
// TYPES: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[STATIC]]
// TYPES: unchecked_addr_cast [[ACCESS]] : $*Builtin.FixedArray<5000, Int32> to $*InlineArray<5000, Int32>
// TYPES: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[SMALL]]
// TYPES: unchecked_addr_cast [[ACCESS]] : $*(Int32, Int32, Int32, Int32) to $*InlineArray<4, Int32>
func testHugeGlobalAndStatic(_ huge: InlineArray<5000, Int32>,
                             _ small: InlineArray<4, Int32>) {
  hugeGlobalArray = huge
  StructWithStaticArrays.huge = huge
  StructWithStaticArrays.huge[1] = 3
  StructWithStaticArrays.small = small
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
