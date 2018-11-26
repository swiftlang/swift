// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership -enable-resilience %s | %FileCheck -check-prefix=CHECK-RESILIENT %s

public enum E: Int {
  case a, b, c
}

// CHECK-DAG: sil [serialized] @$S22enum_raw_representable1EO0B5ValueACSgSi_tcfC
// CHECK-DAG: sil [serialized] @$S22enum_raw_representable1EO0B5ValueSivg

// CHECK-RESILIENT-DAG: sil @$S22enum_raw_representable1EO0B5ValueACSgSi_tcfC
// CHECK-RESILIENT-DAG: sil @$S22enum_raw_representable1EO0B5ValueSivg
