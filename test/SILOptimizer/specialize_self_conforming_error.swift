// RUN: %target-swift-frontend -module-name specialize_self_conforming -emit-sil -O -primary-file %s | %FileCheck %s

// Test 1: apply the substitution
//   [U:Error => Error:Error]
// to the type argument map
//   [T:Error => U:Error]
// on the call to takesError.

@_optimize(none)
func takesError<T : Error>(_: T) {}

@inline(__always)
func callsTakesError<U : Error>(_ error: U) {
  takesError(error)
}

// CHECK-LABEL: sil hidden @$s26specialize_self_conforming5test1yys5Error_pF : $@convention(thin) (@guaranteed Error) -> () {
// CHECK: [[TEMP:%.*]] = alloc_stack $Error
// CHECK: store %0 to [[TEMP]]
// CHECK: [[FN:%.*]] = function_ref @$s26specialize_self_conforming10takesErroryyxs0E0RzlF : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply [[FN]]<Error>([[TEMP]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@in_guaranteed τ_0_0) -> ()
// CHECK: dealloc_stack [[TEMP]]
// CHECK: return

func test1(_ error: Error) {
  callsTakesError(error)
}

// Test 2: apply the substitution
//   [U => Int]
// to the type argument map
//   [T:Error => Error:Error]
// on the call to takesError.

@inline(__always)
func callsTakesErrorWithError<U>(_ error: Error, _ value : U) {
  takesError(error)
}

// CHECK-LABEL: sil hidden @$s26specialize_self_conforming5test2yys5Error_pF : $@convention(thin) (@guaranteed Error) -> () {
// CHECK: [[TEMP:%.*]] = alloc_stack $Error
// CHECK: store %0 to [[TEMP]]
// CHECK: [[FN:%.*]] = function_ref @$s26specialize_self_conforming10takesErroryyxs0E0RzlF : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply [[FN]]<Error>([[TEMP]]) : $@convention(thin) <τ_0_0 where τ_0_0 : Error> (@in_guaranteed τ_0_0) -> ()
// CHECK: dealloc_stack [[TEMP]]
// CHECK: return

func test2(_ error: Error) {
  callsTakesErrorWithError(error, 0)
}

// Test 3: apply the substitution
//   [V => Int]
// to the type argument map
//   [T:Error => Error:Error, U => V]
// on the call to takesErrorAndValue.

@_optimize(none)
func takesErrorAndValue<T : Error, U>(_: T, _: U) {}

@inline(__always)
func callsTakesErrorAndValueWithError<U>(_ error: Error, _ value : U) {
  takesErrorAndValue(error, value)
}

// CHECK-LABEL: sil hidden @$s26specialize_self_conforming5test3yys5Error_pF : $@convention(thin) (@guaranteed Error) -> () {
// CHECK: [[TEMP:%.*]] = alloc_stack $Error
// CHECK: store %0 to [[TEMP]]
// CHECK: [[FN:%.*]] = function_ref @$s26specialize_self_conforming18takesErrorAndValueyyx_q_ts0E0Rzr0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Error> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_1) -> ()
// CHECK: apply [[FN]]<Error, Int>([[TEMP]], {{%.*}}) :
// CHECK: dealloc_stack [[TEMP]]
// CHECK: return

func test3(_ error: Error) {
  callsTakesErrorAndValueWithError(error, 0)
}

// Test 4: just clone the type argument map
//   [Self:P => opened:P, T:Error => Error:Error]
// When the inliner is cloning a substitution map that includes an opened
// archetype, it uses a substitution function that expects not to be called
// on types it's not expecting.

protocol P {}
extension P {
  @_optimize(none)
  func exTakesError<T: Error>(_: T) {}
}

@inline(__always)
func callsExTakesErrorWithError(_ p: P, _ error: Error) {
  p.exTakesError(error)
}

func test4(_ p: P, _ error: Error) {
  callsExTakesErrorWithError(p, error)
}
