// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s | %FileCheck %s

@_silgen_name("foo")
public func foo(_ x: Float, _ y: Float) -> Float {
  return x * y
}

@_silgen_name("foo_indir_ret")
public func foo_indir_ret<T>(x: Float, y: Float, t: T) -> (T, T) {
  return (t, t)
}

@_silgen_name("foo_generic_diff")
public func foo_generic_diff<T : Differentiable>(x: T, y: T) -> T {
  return x
}

let _ = #gradient(of: foo)
let _ = #gradient(of: foo, withRespectTo: .0)

// TODO: Uncomment these when generics are supported.
// let _: (Float, Float) -> (Float, Float) = #gradient(of: foo_generic_diff)
// let _: (Float, Float, [Int]) -> (Float, Float) = #gradient(of: foo_indir_ret, withRespectTo: .0, .1)

// CHECK-LABEL: sil @main :
// CHECK: %{{[0-9]+}} = gradient [wrt 0, 1] %{{[0-9]+}} : $@callee_guaranteed (Float, Float) -> Float
// CHECK: %{{[0-9]+}} = gradient [wrt 0] %{{[0-9]+}} : $@callee_guaranteed (Float, Float) -> Float
