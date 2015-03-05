// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil hidden @_TFO22enum_generic_raw_value1ECU__fMGS0_Q__FT8rawValueSi_GSqGS0_Q___
enum E<T>: Int {
  case A = 1
}

// CHECK-LABEL: sil hidden @_TFO22enum_generic_raw_value1FCUSs9EquatableSs25IntegerLiteralConvertible_USs33_BuiltinIntegerLiteralConvertible__fMGS0_Q__FT8rawValueQ__GSqGS0_Q___
enum F<T: IntegerLiteralConvertible where T: Equatable>: T {
  case A = 1
}
