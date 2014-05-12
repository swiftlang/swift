// FIXME: depends on brittle stdlib implementation details
// XFAIL: *
// RUN: %swift %s -emit-sil -O3 -o - -sil-debug-serialization | FileCheck %s

// CHECK-DAG: sil public_external [transparent] @_TFSsoi1pU__FT3lhsGVSs13UnsafePointerQ__3rhsSi_GS_Q__ : $@thin <T> (UnsafePointer<T>, Int) -> UnsafePointer<T> {
// CHECK-DAG: sil public_external @_TFVSs13UnsafePointerCU__fMGS_Q__FT5valueBp_GS_Q__ : $@thin <T> (Builtin.RawPointer, @thin UnsafePointer<T>.Type) -> UnsafePointer<T> {
// CHECK-DAG: sil public_external @_TFVSs13UnsafePointer4nullU__fMGS_Q__FT_GS_Q__ : $@thin <T> (@thin UnsafePointer<T>.Type) -> UnsafePointer<T> {
// CHECK-DAG: sil public_external @_TFSs25_writeLineNumberToConsoleFT4lineSu_T_ : $@thin (UInt) -> () {
// CHECK-DAG: sil public_external @_TFVSs13UnsafePointerCU__fMGS_Q__FT_GS_Q__ : $@thin <T> (@thin UnsafePointer<T>.Type) -> UnsafePointer<T> {

import Swift

func f(x : UInt8[]) -> UInt8 {
  return x[0]
}
