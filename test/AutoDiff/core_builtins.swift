// RUN: %target-swift-frontend -parse-stdlib -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck -check-prefix=CHECK-SIL %s

import Swift

func evaldiff<T: Differentiable, U: Differentiable>(_ f: @autodiff (T) -> U, _ x: T) {
  let jvp: (T) -> (U, (T.TangentVector) -> U.TangentVector) = Builtin.autodiffGetJVP(f)
  let vjp: (T) -> (U, (U.CotangentVector) -> T.CotangentVector) = Builtin.autodiffGetVJP(f)
  _ = jvp(x)
  _ = vjp(x)
}

// CHECK-SIL-LABEL: @{{.*}}evaldiff{{.*}}
// CHECK-SIL: bb0([[DIFFED:%.*]] : @trivial $@autodiff @noescape @callee_guaranteed (@in_guaranteed T) -> @out U, [[X:%.*]] : @trivial $*T):
// CHECK-SIL:   [[JVP:%.*]] = autodiff_function_extract [jvp] [order 1] [[DIFFED]] : $@autodiff @noescape @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   [[VJP:%.*]] = autodiff_function_extract [vjp] [order 1] [[DIFFED]] : $@autodiff @noescape @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   apply [[JVP]]({{%.*}}, [[X]]) : $@noescape @callee_guaranteed (@in_guaranteed T) -> (@out U, @owned @callee_guaranteed (@in_guaranteed T.TangentVector) -> @out U.TangentVector)
// CHECK-SIL:   apply [[VJP]]({{%.*}}, [[X]]) : $@noescape @callee_guaranteed (@in_guaranteed T) -> (@out U, @owned @callee_guaranteed (@in_guaranteed U.CotangentVector) -> @out T.CotangentVector)
