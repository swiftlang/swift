// RUN: %swift %s -O3 -emit-sil -sil-devirt-threshold 500 -sil-inline-threshold 0 | FileCheck %s
//
// TODO: Once we mangle properly, we can check the actual type of the
// specializations.

class AClass {
  var v: Int = 0
  func inc() { ++v }
}

func mutate(a1: AClass, a2: AClass) {
  a1.inc()
  a2.inc()
}

// wrapper() now calls a specialized mutate()
//CHECK-LABEL: sil @_TF4main7wrapperFT2a1CS_6AClass_T_
//CHECK: function_ref @_TF4main6mutateFT2a1CS_6AClass2a2S0__T__argspec0
func wrapper(a1: AClass) {
  var a2 = AClass()
  mutate(a1, a2)
}

// top_level_function() now calls a specialized wrapper()
//CHECK-LABEL: sil @_TF4main18top_level_functionFT_Si
//CHECK: function_ref @_TF4main7wrapperFT2a1CS_6AClass_T__argspec0
func top_level_function() -> Int {
  var a = AClass()
  wrapper(a)
  return a.v
}

// Specialized wrapper calls a "doubly" specialized mutate()
//CHECK-LABEL: sil private @_TF4main7wrapperFT2a1CS_6AClass_T__argspec0
//CHECK: function_ref @_TF4main6mutateFT2a1CS_6AClass2a2S0__T__argspec0_argspec0
