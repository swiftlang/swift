// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden @_T022enum_generic_raw_value1EO
enum E<T>: Int {
  case A = 1
}

// CHECK-LABEL: sil hidden @_T022enum_generic_raw_value1FO
enum F<T: ExpressibleByIntegerLiteral where T: Equatable>: T {
  case A = 1
}
