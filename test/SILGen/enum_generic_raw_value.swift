// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden [ossa] @$s22enum_generic_raw_value1EO
enum E<T>: Int {
  case a = 1
}

// CHECK-LABEL: sil hidden [ossa] @$s22enum_generic_raw_value1FO
enum F<T: ExpressibleByIntegerLiteral>: T where T: Equatable {
  case a = 1
}
