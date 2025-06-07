// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -disable-availability-checking -o %t -enable-library-evolution -module-name ResilientLibrary %S/Inputs/opaque_values_silgen_resilient.swift
// RUN: %target-swift-frontend -enable-sil-opaque-values -Xllvm -sil-print-types -emit-silgen -disable-availability-checking -I %t %s | %FileCheck %s

import ResilientLibrary

// Verify that an enum instruction producing a trivial is _still_ destroyed if
// (1) the enum instruction's type is non-trivial
// (2) the enum is address-only
// Necessary because such an enum will be lowered to
// init_enum_data_addr/inject_enum_addr and there isn't generally enough
// information to determine whether a destroy_addr is required so it is always
// required.
// CHECK-LABEL: sil [ossa] @produceSomeEmptyNontrivialAddronlyEnumInstance : {{.*}} {
// CHECK:         [[EMPTY_CASE:%[^,]+]] = enum $EnumNontrivialWithEmptyCases, #EnumNontrivialWithEmptyCases.empty!enumelt
// CHECK:         [[SOME_EMPTY_CASE:%[^,]+]] = enum $Optional<EnumNontrivialWithEmptyCases>, #Optional.some!enumelt, [[EMPTY_CASE]]
// CHECK:         destroy_value [[SOME_EMPTY_CASE]]
// CHECK-LABEL: } // end sil function 'produceSomeEmptyNontrivialAddronlyEnumInstance'
@_silgen_name("produceSomeEmptyNontrivialAddronlyEnumInstance")
public func produceSomeEmptyNontrivialAddronlyEnumInstance(_ one: EnumNontrivialWithEmptyCases?) {
  if one != .empty {
  }
}

// CHECK-LABEL: sil [ossa] @produceNoneEmptyAddronlyEnumInstance : {{.*}} {
// CHECK:         [[NONE:%[^,]+]] = enum $Optional<EnumNontrivialWithEmptyCases>, #Optional.none!enumelt 
// CHECK:         return [[NONE]] : $Optional<EnumNontrivialWithEmptyCases> 
// CHECK-LABEL: } // end sil function 'produceNoneEmptyAddronlyEnumInstance'
@_silgen_name("produceNoneEmptyAddronlyEnumInstance")
public func produceNoneEmptyAddronlyEnumInstance() -> EnumNontrivialWithEmptyCases? {
  return .none
}
