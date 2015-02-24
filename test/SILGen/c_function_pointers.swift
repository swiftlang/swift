// RUN: %target-swift-frontend -emit-silgen -enable-c-function-pointers %s | FileCheck %s

func values(arg: @cc(cdecl) Int -> Int) -> @cc(cdecl) Int -> Int {
  return arg
}
// CHECK-LABEL: sil hidden @_TF19c_function_pointers6valuesFcSiSicSiSi
// CHECK:       bb0(%0 : $@cc(cdecl) @thin (Int) -> Int):
// CHECK:         return %0 : $@cc(cdecl) @thin (Int) -> Int

func calls(arg: @cc(cdecl) Int -> Int, x: Int) -> Int {
  return arg(x)
}
// CHECK-LABEL: sil hidden @_TF19c_function_pointers5callsFTcSiSiSi_Si
// CHECK:       bb0(%0 : $@cc(cdecl) @thin (Int) -> Int, %1 : $Int):
// CHECK:         [[RESULT:%.*]] = apply %0(%1)
// CHECK:         return [[RESULT]]

