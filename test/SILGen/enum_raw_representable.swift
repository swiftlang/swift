// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -emit-sorted-sil %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -emit-sorted-sil -enable-library-evolution %s | %FileCheck -check-prefix=CHECK-RESILIENT %s

public enum E: Int {
  case a, b, c
}

// CHECK-LABEL: sil [serialized] [ossa] @$s22enum_raw_representable1EO0B5ValueACSgSi_tcfC

// CHECK-LABEL: sil [serialized] [ossa] @$s22enum_raw_representable1EO0B5ValueSivg
// CHECK: switch_enum %0 : $E
// CHECK: end sil function '$s22enum_raw_representable1EO0B5ValueSivg'


// CHECK-RESILIENT-DAG: sil [ossa] @$s22enum_raw_representable1EO0B5ValueACSgSi_tcfC
// CHECK-RESILIENT-DAG: sil [ossa] @$s22enum_raw_representable1EO0B5ValueSivg
