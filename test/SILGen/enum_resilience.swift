
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_enum.swiftmodule -module-name=resilient_enum -I %t %S/../Inputs/resilient_enum.swift
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name enum_resilience -I %t -enable-library-evolution %s | %FileCheck %s

import resilient_enum

// Resilient enums are always address-only, and switches must include
// a default case

// CHECK-LABEL: sil hidden [ossa] @$s15enum_resilience15resilientSwitchyy0c1_A06MediumOF : $@convention(thin) (@in_guaranteed Medium) -> ()
// CHECK:         [[BOX:%.*]] = alloc_stack $Medium
// CHECK-NEXT:    copy_addr %0 to [init] [[BOX]]
// CHECK-NEXT:    [[METATYPE:%.+]] = value_metatype $@thick Medium.Type, [[BOX]] : $*Medium
// CHECK-NEXT:    switch_enum_addr [[BOX]] : $*Medium, case #Medium.Paper!enumelt: bb1, case #Medium.Canvas!enumelt: bb2, case #Medium.Pamphlet!enumelt: bb3, case #Medium.Postcard!enumelt: bb4, default bb5
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

// CHECK-LABEL: sil hidden [ossa] @$s15enum_resilience22resilientSwitchDefaultys5Int32V0c1_A06MediumOF : $@convention(thin) (@in_guaranteed Medium) -> Int32 {
func resilientSwitchDefault(_ m: Medium) -> Int32 {
  // CHECK: switch_enum_addr %2 : $*Medium, case #Medium.Paper!enumelt: [[PAPER:[^ ]+]], case #Medium.Canvas!enumelt: [[CANVAS:[^ ]+]], default [[DEFAULT:[^ ]+]]
  switch m {
  // CHECK: [[PAPER]]:
  // CHECK: integer_literal $Builtin.IntLiteral, 0
  case .Paper: return 0
  // CHECK: [[CANVAS]]:
  // CHECK: integer_literal $Builtin.IntLiteral, 1
  case .Canvas: return 1
  // CHECK: [[DEFAULT]]:
  // CHECK: integer_literal $Builtin.IntLiteral, -1
  default: return -1
  }
} // CHECK: end sil function '$s15enum_resilience22resilientSwitchDefaultys5Int32V0c1_A06MediumOF'

// CHECK-LABEL: sil hidden [ossa] @$s15enum_resilience26resilientSwitchUnknownCaseys5Int32V0c1_A06MediumOF : $@convention(thin) (@in_guaranteed Medium) -> Int32 {
func resilientSwitchUnknownCase(_ m: Medium) -> Int32 {
  // CHECK: switch_enum_addr %2 : $*Medium, case #Medium.Paper!enumelt: [[PAPER:[^ ]+]], case #Medium.Canvas!enumelt: [[CANVAS:[^ ]+]], default [[DEFAULT:[^ ]+]]
  switch m {
  // CHECK: [[PAPER]]:
  // CHECK: integer_literal $Builtin.IntLiteral, 0
  case .Paper: return 0
  // CHECK: [[CANVAS]]:
  // CHECK: integer_literal $Builtin.IntLiteral, 1
  case .Canvas: return 1
  // CHECK: [[DEFAULT]]:
  // CHECK: integer_literal $Builtin.IntLiteral, -1
  @unknown case _: return -1
  }
} // CHECK: end sil function '$s15enum_resilience26resilientSwitchUnknownCaseys5Int32V0c1_A06MediumOF'

// CHECK-LABEL: sil hidden [ossa] @$s15enum_resilience36resilientSwitchUnknownCaseExhaustiveys5Int32V0c1_A06MediumOF : $@convention(thin) (@in_guaranteed Medium) -> Int32 {
func resilientSwitchUnknownCaseExhaustive(_ m: Medium) -> Int32 {
  // CHECK: switch_enum_addr %2 : $*Medium, case #Medium.Paper!enumelt: [[PAPER:[^ ]+]], case #Medium.Canvas!enumelt: [[CANVAS:[^ ]+]], case #Medium.Pamphlet!enumelt: [[PAMPHLET:[^ ]+]], case #Medium.Postcard!enumelt: [[POSTCARD:[^ ]+]], default [[DEFAULT:[^ ]+]]
  switch m {
  // CHECK: [[PAPER]]:
  // CHECK: integer_literal $Builtin.IntLiteral, 0
  case .Paper: return 0
  // CHECK: [[CANVAS]]:
  // CHECK: integer_literal $Builtin.IntLiteral, 1
  case .Canvas: return 1
  // CHECK: [[PAMPHLET]]:
  // CHECK: integer_literal $Builtin.IntLiteral, 2
  case .Pamphlet: return 2
  // CHECK: [[POSTCARD]]:
  // CHECK: integer_literal $Builtin.IntLiteral, 3
  case .Postcard: return 3
  // CHECK: [[DEFAULT]]:
  // CHECK: integer_literal $Builtin.IntLiteral, -1
  @unknown case _: return -1
  }
}

// Indirect enums are still address-only, because the discriminator is stored
// as part of the value, so we cannot resiliently make assumptions about the
// enum's size

// CHECK-LABEL: sil hidden [ossa] @$s15enum_resilience21indirectResilientEnumyy010resilient_A016IndirectApproachOF : $@convention(thin) (@in_guaranteed IndirectApproach) -> ()
func indirectResilientEnum(_ ia: IndirectApproach) {}

public enum MyResilientEnum {
  case kevin
  case loki

  // CHECK-LABEL: sil hidden [ossa] @$s15enum_resilience15MyResilientEnumOACycfC : $@convention(method) (@thin MyResilientEnum.Type) -> @out MyResilientEnum
  // CHECK:       [[SELF_BOX:%.*]] = alloc_box ${ var MyResilientEnum }, var, name "self"
  // CHECK:       [[SELF_TMP:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]] : ${ var MyResilientEnum }
  // CHECK:       [[LIFETIME:%.*]] = begin_borrow [var_decl] [[SELF_TMP]]
  // CHECK:       [[SELF_ADDR:%.*]] = project_box [[LIFETIME]] : ${ var MyResilientEnum }, 0
  // CHECK:       [[NEW_SELF:%.*]] = enum $MyResilientEnum, #MyResilientEnum.loki!enumelt
  // CHECK:       [[ACCESS:%.*]] = begin_access [modify] [unknown] [[SELF_ADDR]] : $*MyResilientEnum
  // CHECK:       assign [[NEW_SELF]] to [[ACCESS]] : $*MyResilientEnum
  // CHECK:       end_access [[ACCESS]] : $*MyResilientEnum
  // CHECK:       copy_addr [[SELF_ADDR]] to [init] %0 : $*MyResilientEnum
  // CHECK:       destroy_value [[SELF_TMP]] : ${ var MyResilientEnum }
  // CHECK:       return
  init() {
    self = .loki
  }

  // CHECK-LABEL: sil hidden [ossa] @$s15enum_resilience15MyResilientEnumO9getAHorseACyFZ : $@convention(method) (@thin MyResilientEnum.Type) -> @out MyResilientEnum
  // CHECK:       [[NEW_SELF:%.*]] = enum $MyResilientEnum, #MyResilientEnum.loki!enumelt
  // CHECK:       store [[NEW_SELF]] to [trivial] %0 : $*MyResilientEnum
  // CHECK:       return
  static func getAHorse() -> MyResilientEnum {
    return .loki
  }
}

public enum MoreHorses {
  case marshall(Int)
  case seuss(AnyObject)

  // CHECK-LABEL: sil hidden [ossa] @$s15enum_resilience10MoreHorsesOACycfC : $@convention(method) (@thin MoreHorses.Type) -> @out MoreHorses
  // CHECK:       [[SELF_BOX:%.*]] = alloc_box ${ var MoreHorses }, var, name "self"
  // CHECK:       [[SELF_TMP:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]] : ${ var MoreHorses }
  // CHECK:       [[SELF_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[SELF_TMP]]
  // CHECK:       [[SELF_ADDR:%.*]] = project_box [[SELF_LIFETIME]] : ${ var MoreHorses }, 0
  // CHECK:       [[BUILTIN_INT:%.*]] = integer_literal $Builtin.IntLiteral, 0
  // CHECK:       [[INT_METATYPE:%.*]] = metatype $@thin Int.Type
  // CHECK:       [[INT_CTOR:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
  // CHECK:       [[PAYLOAD:%.*]] = apply [[INT_CTOR]]([[BUILTIN_INT]], [[INT_METATYPE]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
  // CHECK:       [[NEW_SELF:%.*]] = enum $MoreHorses, #MoreHorses.marshall!enumelt, [[PAYLOAD]] : $Int
  // CHECK:       [[ACCESS:%.*]] = begin_access [modify] [unknown] [[SELF_ADDR]] : $*MoreHorses
  // CHECK:       assign [[NEW_SELF]] to [[ACCESS]] : $*MoreHorses
  // CHECK:       end_access [[ACCESS]] : $*MoreHorses
  // CHECK:       copy_addr [[SELF_ADDR]] to [init] %0 : $*MoreHorses
  // CHECK:       destroy_value [[SELF_TMP]] : ${ var MoreHorses }
  // CHECK:       return
  init() {
    self = .marshall(0)
  }
}

public func referenceCaseConstructors() {
  _ = MoreHorses.marshall
  _ = MoreHorses.seuss
}

// CHECK-LABEL: sil [ossa] @$s15enum_resilience15resilientSwitchyyAA15MyResilientEnumOF : $@convention(thin) (@in_guaranteed MyResilientEnum) -> ()
// CHECK:      [[ENUM:%.*]] = load [trivial] %0
// CHECK:      switch_enum [[ENUM]] : $MyResilientEnum, case #MyResilientEnum.kevin!enumelt: bb1, case #MyResilientEnum.loki!enumelt: bb2
// CHECK:      return
public func resilientSwitch(_ e: MyResilientEnum) {
  switch e {
  case .kevin: ()
  case .loki: ()
  }
}

// CHECK-LABEL: sil [ossa] @$s15enum_resilience15resilientIfCaseySbAA15MyResilientEnumOF : $@convention(thin) (@in_guaranteed MyResilientEnum) -> Bool
// CHECK:       [[ENUM:%.*]] = load [trivial] %0 : $*MyResilientEnum
// CHECK:       switch_enum [[ENUM]] : $MyResilientEnum, case #MyResilientEnum.kevin!enumelt: bb1, case #MyResilientEnum.loki!enumelt: bb2
// CHECK:       return
public func resilientIfCase(_ e: MyResilientEnum) -> Bool {
  if case .kevin = e {
    return true
  } else {
    return false
  }
}

// Inlinable functions must lower the switch as if it came from outside the module

// CHECK-LABEL: sil [serialized] [ossa] @$s15enum_resilience15inlinableSwitchyyAA15MyResilientEnumOF : $@convention(thin) (@in_guaranteed MyResilientEnum) -> ()
// CHECK:      [[ENUM:%.*]] = alloc_stack $MyResilientEnum
// CHECK:      copy_addr %0 to [init] [[ENUM]] : $*MyResilientEnum
// CHECK:      switch_enum_addr [[ENUM]] : $*MyResilientEnum, case #MyResilientEnum.kevin!enumelt: bb1, case #MyResilientEnum.loki!enumelt: bb2, default bb3
// CHECK:      return
@inlinable public func inlinableSwitch(_ e: MyResilientEnum) {
  switch e {
  case .kevin: ()
  case .loki: ()
  }
}
