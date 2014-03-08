// RUN: %swift %s -emit-sil -O3 -o - -sil-debug-serialization | FileCheck %s

// CHECK: sil public_external @_TFSs25_writeLineNumberToConsoleFT4lineSu_T_ : $@thin (UInt) -> () {
// CHECK: sil public_external @_TFVSs13UnsafePointer6isNullU__fGS_Q__FT_Sb : $@cc(method) @thin <T> (UnsafePointer<T>) -> Bool {
// CHECK: sil public_external @_TFSsoi1pU__FT3lhsGVSs13UnsafePointerQ__3rhsSi_GS_Q__ : $@thin <T> (UnsafePointer<T>, Int) -> UnsafePointer<T> {
// CHECK: sil public_external @_TFVSs13UnsafePointerCU__fMGS_Q__FT5valueBp_GS_Q__ : $@thin <T> (Builtin.RawPointer, @thin UnsafePointer<T>.Type) -> UnsafePointer<T> {
// CHECK: sil public_external @_TFSsoi2eeU__FT3lhsGVSs13UnsafePointerQ__3rhsGS_Q___Sb : $@thin <T> (UnsafePointer<T>, UnsafePointer<T>) -> Bool {
// CHECK: sil public_external @_TFVSs13UnsafePointer4nullU__fMGS_Q__FT_GS_Q__ : $@thin <T> (@thin UnsafePointer<T>.Type) -> UnsafePointer<T> {
// CHECK: sil public_external @_TFVSs13UnsafePointerCU__fMGS_Q__FT_GS_Q__ : $@thin <T> (@thin UnsafePointer<T>.Type) -> UnsafePointer<T> {

import Swift

func f(x : UInt8[]) -> UInt8 {
  return x[0]
}
