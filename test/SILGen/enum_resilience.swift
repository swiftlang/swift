// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -I %t -emit-silgen -enable-resilience %s | %FileCheck %s

import resilient_enum

// Resilient enums are always address-only, and switches must include
// a default case

// CHECK-LABEL: sil hidden @_T015enum_resilience15resilientSwitchy0c1_A06MediumOF : $@convention(thin) (@in Medium) -> ()
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
// CHECK-NEXT:    unreachable
// CHECK:       bb6:
// CHECK-NEXT:    destroy_addr %0
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

// Indirect enums are still address-only, because the discriminator is stored
// as part of the value, so we cannot resiliently make assumptions about the
// enum's size

// CHECK-LABEL: sil hidden @_T015enum_resilience21indirectResilientEnumy010resilient_A016IndirectApproachOF : $@convention(thin) (@in IndirectApproach) -> ()
func indirectResilientEnum(_ ia: IndirectApproach) {}
