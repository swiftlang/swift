// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

@_transparent
public func gradient<T : Differentiable, R : FloatingPoint>(
  of f: /*@autodiff*/ @escaping (T) -> R
) -> (T) -> T {
  return #gradient(f)
}

// CHECK-LABEL: @{{.*}}gradient{{.*}}
// CHECK:   [[GRAD:%.*]] = gradient [source 0] [wrt 0] {{%.*}}
// CHECK:   return [[GRAD]] : $@callee_guaranteed (@in_guaranteed T) -> @out T

@_transparent
public func gradient2<T : Differentiable, R : FloatingPoint>(
  at x: T, in f: /*@autodiff*/ @escaping (T) -> R
) -> T {
  return #gradient(f)(x)
}

// CHECK-LABEL: @{{.*}}gradient2{{.*}}
// CHECK:   [[GRAD:%.*]] = gradient [source 0] [wrt 0] {{%.*}}
// CHECK:   [[BORROWED_GRAD:%.*]] = begin_borrow [[GRAD]]
// CHECK:   apply [[BORROWED_GRAD]]
