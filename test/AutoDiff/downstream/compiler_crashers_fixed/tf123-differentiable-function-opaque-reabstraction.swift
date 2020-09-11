// RUN: %target-swift-frontend-emit-silgen %s
// REQUIRES: asserts

// TF-123: SILGen crashes when reabstracting `@differentiable` functions to
// opaque abstraction patterns.
// The culprit is `createAutoDiffThunk` in lib/SILGen/SILGenPoly.cpp.

// Reproducer: cast `@differentiable` function-typed value to `Any`.
let function: @differentiable (Float) -> Float
_ = function as Any

// SIL verification failed: JVP type does not match expected JVP type
//   $@callee_guaranteed (@in_guaranteed Float) -> @out (Float, @callee_guaranteed (@in_guaranteed Float) -> @out Float)
//   $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)

// Reproducer: create key path to `@differentiable` function-typed value.
struct TF_123: KeyPathIterable {
  let function: @differentiable (Float) -> Float
}
_ = \TF_123.function

// SIL verification failed: JVP type does not match expected JVP type
//   $@callee_guaranteed (@in_guaranteed Float) -> @out (Float, @callee_guaranteed (@in_guaranteed Float) -> @out Float)
//   $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
