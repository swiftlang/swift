// RUN: %target-swift-frontend %s -emit-silgen -o - | FileCheck %s

func standalone_generic<T>(x: T, y: T) -> T { return x }

// CHECK: sil hidden @_TF16specialize_thunk21return_specialization
func return_specialization() -> (x: Int, y: Int) -> Int {
// CHECK:      [[T0:%.*]] = function_ref @_TF16specialize_thunk18standalone_genericU__FTQ_Q__Q_ : $@thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @in τ_0_0) -> ()
// CHECK-NEXT: [[T1:%.*]] = partial_apply [[T0]]<Int>() : $@thin <τ_0_0> (@out τ_0_0, @in τ_0_0, @in τ_0_0) -> ()
// CHECK:      [[T2:%.*]] = function_ref @[[THUNK:.*]] : $@thin (Int, Int, @owned @callee_owned (@out Int, @in Int, @in Int) -> ()) -> Int
// CHECK-NEXT: [[T3:%.*]] = partial_apply [[T2]]([[T1]])
// CHECK-NEXT: return [[T3]] : $@callee_owned (Int, Int) -> Int
  return standalone_generic
}

