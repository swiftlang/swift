
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift -enable-testing
// RUN: %target-swift-emit-silgen -module-name enum_resilience_testable -I %t -enable-sil-ownership -enable-nonfrozen-enum-exhaustivity-diagnostics -swift-version 5 %s | %FileCheck %s

// This is mostly testing the same things as enum_resilienc.swift, just in a
// context where the user will never be forced to write a default case. It's the
// same effect as -swift-version 4 and ignoring the exhaustivity warning,
// though.

@testable import resilient_enum

// Resilient enums are always address-only, and switches must include
// a default case

// CHECK-LABEL: sil hidden @$s24enum_resilience_testable15resilientSwitchyy0d1_A06MediumOF : $@convention(thin) (@in_guaranteed Medium) -> ()
// CHECK:         [[BOX:%.*]] = alloc_stack $Medium
// CHECK-NEXT:    copy_addr %0 to [initialization] [[BOX]]
// CHECK-NEXT:    switch_enum_addr [[BOX]] : $*Medium, case #Medium.Paper!enumelt: bb1, case #Medium.Canvas!enumelt: bb2, case #Medium.Pamphlet!enumelt.1: bb3, case #Medium.Postcard!enumelt.1: bb4, default bb5
// CHECK:       bb1:
// CHECK-NEXT:    dealloc_stack [[BOX]]
// CHECK-NEXT:    br bb6
// CHECK:       bb2:
// CHECK-NEXT:    dealloc_stack [[BOX]]
// CHECK-NEXT:    br bb6
// CHECK:       bb3:
// CHECK-NEXT:    [[INDIRECT_ADDR:%.*]] = unchecked_take_enum_data_addr [[BOX]]
// CHECK-NEXT:    [[INDIRECT:%.*]] = load [take] [[INDIRECT_ADDR]]
// CHECK-NEXT:    [[PAYLOAD:%.*]] = project_box [[INDIRECT]]
// CHECK-NEXT:    destroy_value [[INDIRECT]]
// CHECK-NEXT:    dealloc_stack [[BOX]]
// CHECK-NEXT:    br bb6
// CHECK:       bb4:
// CHECK-NEXT:    [[PAYLOAD_ADDR:%.*]] = unchecked_take_enum_data_addr [[BOX]]
// CHECK-NEXT:    destroy_addr [[PAYLOAD_ADDR]]
// CHECK-NEXT:    dealloc_stack [[BOX]]
// CHECK-NEXT:    br bb6
// CHECK:       bb5:
// CHECK-NEXT:    [[METATYPE:%.+]] = value_metatype $@thick Medium.Type, [[BOX]] : $*Medium
// CHECK-NEXT:    // function_ref
// CHECK-NEXT:    [[DIAGNOSE:%.+]] = function_ref @$ss27_diagnoseUnexpectedEnumCase
// CHECK-NEXT:    = apply [[DIAGNOSE]]<Medium>([[METATYPE]]) : $@convention(thin) <τ_0_0> (@thick τ_0_0.Type) -> Never
// CHECK-NEXT:    unreachable
// CHECK:       bb6:
// CHECK-NOT:    destroy_addr %0
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:    return [[RESULT]]

func resilientSwitch(_ m: Medium) {
  switch m {
  case .Paper: ()
  case .Canvas: ()
  case .Pamphlet: ()
  case .Postcard: ()
  }
}
