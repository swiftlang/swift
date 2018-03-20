// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s | %FileCheck %s

@_silgen_name("foo")
public func foo(_ x: Float) -> Float {
  return x * x
}

/// FIXME: Test tuple flattening once Sema supports tuple differentiation arguments.
// @_silgen_name("foo_tuple")
// public func foo_tuple(_ x: ((Float, Float)), _ y: (Float, (Float, (Float)))) -> Float {
//   return 0
// }

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

// CHECK-LABEL: sil @foo__grad_all : $@convention(thin) (Float) -> Float
// CHECK-NEXT: bb0
// CHECK-NEXT: autodiff_reverse @foo : $@convention(thin) (Float) -> Float

// CHECK-LABEL: sil @foo__grad_0 : $@convention(thin) (Float) -> Float
// CHECK-NEXT: bb0
// CHECK-NEXT: autodiff_reverse [wrt 0] @foo : $@convention(thin) (Float) -> Float
