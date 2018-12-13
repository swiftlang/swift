// RUN: %target-swift-frontend -parse-stdlib -typecheck -verify %s
// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck -check-prefix=CHECK-SIL %s

import Swift

func evaldiff<T: Differentiable, U: Differentiable>(_ f: @escaping @autodiff (T) -> U, _ x: T) {
  let jvp: (T) -> (U, (T.TangentVector) -> U.TangentVector) = Builtin.autodiffGetJVP(f)
  let vjp: (T) -> (U, (U.CotangentVector) -> T.CotangentVector) = Builtin.autodiffGetVJP(f)
  _ = jvp(x)
  _ = vjp(x)
}

// CHECK-SIL-LABEL: @{{.*}}evaldiff{{.*}}
// CHECK-SIL: bb0([[DIFFED:%.*]] : @guaranteed $@autodiff @callee_guaranteed (@in_guaranteed T) -> @out U, [[X:%.*]] : @trivial $*T):
// CHECK-SIL:   [[DIFFED_COPY_1:%.*]] = copy_value [[DIFFED]] : $@autodiff @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   [[JVP:%.*]] = autodiff_function_extract [jvp] [order 1] [[DIFFED_COPY_1]] : $@autodiff @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   [[DIFFED_COPY_2:%.*]] = copy_value [[DIFFED]] : $@autodiff @callee_guaranteed (@in_guaranteed T) -> @out U
// CHECK-SIL:   [[VJP:%.*]] = autodiff_function_extract [vjp] [order 1] [[DIFFED_COPY_2]] : $@autodiff @callee_guaranteed (@in_guaranteed T) -> @out U
