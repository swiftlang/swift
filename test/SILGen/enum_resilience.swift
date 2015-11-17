// RUN: %target-swift-frontend -I %S/../Inputs -enable-source-import -emit-silgen -enable-resilience %s | FileCheck %s

import resilient_enum

// Resilient enums are always address-only, and switches must include
// a default case

// CHECK-LABEL: sil hidden @_TF15enum_resilience15resilientSwitchFO14resilient_enum6MediumT_ : $@convention(thin) (@in Medium) -> ()
// CHECK:         [[BOX:%.*]] = alloc_stack $Medium
// CHECK-NEXT:    copy_addr %0 to [initialization] [[BOX]]#1
// CHECK-NEXT:    switch_enum_addr [[BOX]]#1 : $*Medium, case #Medium.Paper!enumelt: bb1, case #Medium.Canvas!enumelt: bb2, default bb3
// CHECK:       bb1:
// CHECK-NEXT:    dealloc_stack [[BOX]]#0
// CHECK-NEXT:    br bb4
// CHECK:       bb2:
// CHECK-NEXT:    dealloc_stack [[BOX]]#0
// CHECK-NEXT:    br bb4
// CHECK:       bb3:
// CHECK-NEXT:    unreachable
// CHECK:       bb4:
// CHECK-NEXT:    destroy_addr %0
// CHECK-NEXT:    [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:    return [[RESULT]]

func resilientSwitch(m: Medium) {
  switch m {
    case .Paper: ()
    case .Canvas: ()
  }
}
