// FIXME: depends on brittle stdlib implementation details
// XFAIL: *
// RUN: %target-swift-frontend %s -emit-sil -O -o - -sil-debug-serialization | FileCheck %s

// CHECK-DAG: sil public_external [transparent] @_TFSsoi1pU__FT3lhsGVSs13UnsafeMutablePointerQ__3rhsSi_GS_Q__ : $@convention(thin) <T> (UnsafeMutablePointer<T>, Int) -> UnsafeMutablePointer<T> {
// CHECK-DAG: sil public_external @_TFVSs13UnsafeMutablePointerCU__fMGS_Q__FT5valueBp_GS_Q__ : $@convention(thin) <T> (Builtin.RawPointer, @thin UnsafeMutablePointer<T>.Type) -> UnsafeMutablePointer<T> {
// CHECK-DAG: sil public_external @_TFVSs13UnsafeMutablePointer4nullU__fMGS_Q__FT_GS_Q__ : $@convention(thin) <T> (@thin UnsafeMutablePointer<T>.Type) -> UnsafeMutablePointer<T> {
// CHECK-DAG: sil public_external @_TFSs25_writeLineNumberToConsoleFT4lineSu_T_ : $@convention(thin) (UInt) -> () {
// CHECK-DAG: sil public_external @_TFVSs13UnsafeMutablePointerCU__fMGS_Q__FT_GS_Q__ : $@convention(thin) <T> (@thin UnsafeMutablePointer<T>.Type) -> UnsafeMutablePointer<T> {

import Swift

func f(x : UInt8[]) -> UInt8 {
  return x[0]
}
