// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-emit-silgen -emit-sorted-sil %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -emit-sorted-sil -enable-resilience %s | %FileCheck -check-prefix=CHECK-RESILIENT %s

public enum E: Int {
  case a, b
  @available(macOS 500, iOS 500, watchOS 500, tvOS 500, *)
  case c
}

// CHECK-LABEL: sil [serialized] [ossa] @$s32enum_raw_representable_available1EO0B5ValueACSgSi_tcfC
// CHECK: integer_literal $Builtin.Word, 500
// CHECK: end sil function '$s32enum_raw_representable_available1EO0B5ValueACSgSi_tcfC'

// CHECK-LABEL: sil [serialized] [ossa] @$s32enum_raw_representable_available1EO0B5ValueSivg
// CHECK: switch_enum %0 : $E
// CHECK: end sil function '$s32enum_raw_representable_available1EO0B5ValueSivg'


// CHECK-RESILIENT-DAG: sil [ossa] @$s32enum_raw_representable_available1EO0B5ValueACSgSi_tcfC
// CHECK-RESILIENT-DAG: sil [ossa] @$s32enum_raw_representable_available1EO0B5ValueSivg
