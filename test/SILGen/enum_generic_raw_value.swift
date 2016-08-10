// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden @_TFO22enum_generic_raw_value1EC
enum E<T>: Int {
  case A = 1
}

// CHECK-LABEL: sil hidden @_TFO22enum_generic_raw_value1FC
enum F<T: ExpressibleByIntegerLiteral where T: Equatable>: T {
  case A = 1
}
