// RUN: not --crash %target-swift-frontend -parse-stdlib -emit-sil %s 2>&1 | %FileCheck %s
// REQUIRES: asserts

import Swift

protocol FloatTangent : Differentiable
  where TangentVector == Float, CotangentVector == Float {}

func gradient<T: Differentiable, R: FloatTangent>(
  of f: @autodiff (T) -> R, at x: T
) -> T.CotangentVector {
  fatalError("unimplemented")
}

extension Float : FloatTangent {}

func example(_ x: Float) -> Float {
  return x
}

func test<T: FloatTangent>(_ x: T) {
  let _ = gradient(of: example, at: 1)
}

// CHECK: SIL verification failed: Unexpected JVP function type: expectedJVPType == jvpType

// Ideally, the above code would compile and work correctly. This test
// documents the current non-ideal SIL verification failure.
//
// The reabstraction thunk for passing `example` into `gradient` fails SIL
// verification. The reabstracted function types are:
//
//  original: $@noescape @callee_guaranteed (@in_guaranteed Float) -> @out Float
//  JVP: @noescape @callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> Float)
//  VJP: @noescape @callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (Float) -> @out Float)
//
// However, when these get bundled together, SIL verification expects that the
// associated function types are:
//
//  JVP: @noescape @callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
//  VJP: @noescape @callee_guaranteed (@in_guaranteed Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float)
//
// This happens because SIL verification only looks at the reabstracted
// original function type when determining the expected reabstracted associated
// function types, and the reabstracted original function type does not have
// enough information to precisely determine how the associated fuctions have
// been reabstracted.
