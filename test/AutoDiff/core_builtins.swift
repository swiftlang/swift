// RUN: %target-swift-frontend -parse-stdlib -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck -check-prefix=CHECK-SIL %s

import Swift

func evaldiff<T: Differentiable, U: Differentiable>(_ f: @autodiff (T) -> U, _ x: T) -> (U, (T.TangentVector) -> U.TangentVector)
  where T == T.TangentVector {
  return Builtin.autodiffApplyJVP(f, x)
}

// CHECK-SIL-LABEL: @{{.*}}evaldiff{{.*}}
// CHECK-SIL: bb0([[DIFFED:%.*]] : @trivial $@autodiff @noescape @callee_guaranteed (@in_guaranteed T) -> @out U, [[X:%.*]] : @trivial $*T):
// CHECK-SIL:   [[JVP:%.*]] = autodiff_function_extract [jvp] [order 1] [[DIFFED]] : $@autodiff @noescape @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   [[VJP:%.*]] = autodiff_function_extract [vjp] [order 1] [[DIFFED]] : $@autodiff @noescape @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   apply [[JVP]]({{%.*}}, [[X]]) : $@noescape @callee_guaranteed (@in_guaranteed T) -> (@out U, @owned @callee_guaranteed (@in_guaranteed T.TangentVector) -> @out U.TangentVector)
// CHECK-SIL:   apply [[VJP]]({{%.*}}, [[X]]) : $@noescape @callee_guaranteed (@in_guaranteed T) -> (@out U, @owned @callee_guaranteed (@in_guaranteed U.CotangentVector) -> @out T.CotangentVector)

// func valueWithDifferential<T, R>(
//   at x: T, in f: @autodiff (T) -> R
// ) -> (value: R, differential: (T.TangentVector) -> R.TangentVector)
//   where T : Differentiable, R : Differentiable {
//   return Builtin.autodiffApplyJVP(f, x)
// }
// 
// func valueWithPullback<T, R>(
//   at x: T, in f: @autodiff (T) -> R
// ) -> (value: R, pullback: (R.CotangentVector) -> T.CotangentVector)
//   where T : Differentiable, R : Differentiable {
//   return Builtin.autodiffApplyVJP(f, x)
// }
