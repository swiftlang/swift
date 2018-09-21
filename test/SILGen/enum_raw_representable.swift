// RUN: %target-swift-emit-silgen -enable-sil-ownership -emit-sorted-sil %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-sil-ownership -emit-sorted-sil -enable-resilience %s | %FileCheck -check-prefix=CHECK-RESILIENT %s

public enum E: Int {
  case a, b, c
}

// CHECK-LABEL: sil [serialized] @$s22enum_raw_representable1EO0B5ValueACSgSi_tcfC

// CHECK-LABEL: sil [serialized] @$s22enum_raw_representable1EO0B5ValueSivg
// CHECK: switch_enum %0 : $E
// CHECK: end sil function '$s22enum_raw_representable1EO0B5ValueSivg'


// CHECK-RESILIENT-DAG: sil @$s22enum_raw_representable1EO0B5ValueACSgSi_tcfC
// CHECK-RESILIENT-DAG: sil @$s22enum_raw_representable1EO0B5ValueSivg
