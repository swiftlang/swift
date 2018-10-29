// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

// CHECK-LABEL: sil hidden @$s22enum_generic_raw_value1EO
enum E<T>: Int {
  case A = 1
}

// CHECK-LABEL: sil hidden @$s22enum_generic_raw_value1FO
enum F<T: ExpressibleByIntegerLiteral>: T where T: Equatable {
  case A = 1
}
