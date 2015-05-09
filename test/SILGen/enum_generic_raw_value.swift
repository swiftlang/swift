// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil hidden @_TFO22enum_generic_raw_value1ECurfMGS0_q__FT8rawValueSi_GSqGS0_q___
enum E<T>: Int {
  case A = 1
}

// CHECK-LABEL: sil hidden @_TFO22enum_generic_raw_value1FCuRq_Ss9Equatableq_Ss25IntegerLiteralConvertible_fMGS0_q__FT8rawValueq__GSqGS0_q___
enum F<T: IntegerLiteralConvertible where T: Equatable>: T {
  case A = 1
}
