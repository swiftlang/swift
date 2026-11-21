// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/local_type_originally_defined_in_other.swiftmodule %S/Inputs/local_type_originally_defined_in_other.swift
// RUN: %target-swift-frontend -I%t -g -emit-ir %s -o - | %FileCheck %s

// Look up the local types by the names in their debug info.
// RUN: %target-swift-frontend -I%t -g -emit-module -module-name local_type_originally_defined_in -emit-module-path %t/local_type_originally_defined_in.swiftmodule %s
// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/local_type_originally_defined_in.swiftmodule -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE
// REQUIRES: OS=macosx

import local_type_originally_defined_in_other

public func definedInOtherModule() {
  let s = Sheep()
  // CHECK-DAG: DICompositeType(tag: DW_TAG_structure_type, {{.*}}"$s4Barn5SheepCD
}
public func localTypeAliasTest(horse: Horse) {
  // The local type mangling for 'A' mentions 'Horse', which is mangled
  // using its original module name in debug info.
  typealias A = Int

  let info = UnsafeMutablePointer<A>.allocate(capacity: 1)
  _ = info
  // CHECK-DAG: name: "$s32local_type_originally_defined_in0A13TypeAliasTest5horsey4Barn5HorseV_tF1AL_aD"
}

// DEMANGLE-TYPE: $s32local_type_originally_defined_in0A13TypeAliasTest5horsey4Barn5HorseV_tF1AL_aD
// CHECK-TYPE: {{^}}A{{$}}


public func localTypeTest(horse: Horse) {
  // The local type mangling for 'LocalStruct' mentions 'Horse', which is
  // mangled using its original module name in debug info.
  struct LocalStruct {}

  let info = UnsafeMutablePointer<LocalStruct>.allocate(capacity: 1)
  _ = info
  // CHECK-DAG: DICompositeType(tag: DW_TAG_structure_type, {{.*}}: "$s32local_type_originally_defined_in0A8TypeTest5horsey4Barn5HorseV_tF11LocalStructL_VD"
}

// DEMANGLE-TYPE: $s32local_type_originally_defined_in0A8TypeTest5horsey4Barn5HorseV_tF11LocalStructL_VD
// CHECK-TYPE: {{^}}LocalStruct{{$}}

public func localTypeAliasTest() -> Horse {
  typealias B = Int

  let info = UnsafeMutablePointer<B>.allocate(capacity: 1)
  _ = info
  return Horse()
  // CHECK-DAG: name: "$s32local_type_originally_defined_in0A13TypeAliasTest4Barn5HorseVyF1BL_aD"
}

// DEMANGLE-TYPE: $s32local_type_originally_defined_in0A13TypeAliasTest4Barn5HorseVyF1BL_aD
// CHECK-TYPE: {{^}}B{{$}}

public func localTypeAliasTestGeneric<T: Cow>(cow: T) {
  typealias C = Int

  let info = UnsafeMutablePointer<C>.allocate(capacity: 1)
  _ = info
}
