// RUN: %swift %s -emit-silgen -o - | FileCheck %s

func standalone_generic<T>(x:T, y:T) -> T { return x }

// CHECK: sil @_T16specialize_thunk21return_specializationFT_FT1xSi1ySi_Si : $[thin] () -> (x : Int64, y : Int64) -> Int64 {
func return_specialization() -> (x:Int64, y:Int64) -> Int64 {
// CHECK:   %1 = function_ref @_T16specialize_thunk18standalone_genericU__FT1xQ_1yQ__Q_ : $[thin] <T> (x : T, y : T) -> T
// CHECK:   %2 = partial_apply %1<T = Int64>() : $[thin] <T> (x : T, y : T) -> T // user: %3
// CHECK:   return %2 : $(x : Int64, y : Int64) -> Int64
  return standalone_generic
}

