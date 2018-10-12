// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s | %FileCheck %s

//===----------------------------------------------------------------------===//
// Normal types
//===----------------------------------------------------------------------===//

@_silgen_name("foo")
@differentiable(reverse, adjoint: dfoo(_:_:partial:seed:))
public func foo(_ x: Float, _ y: Float) -> Float {
  return 1
}

// CHECK-LABEL: sil [reverse_differentiable source 0 wrt 0, 1 primal @foo adjoint @dfoo primitive] @foo

@_silgen_name("dfoo")
public func dfoo(_ x: Float, _ y: Float, partial: Float, seed: Float) -> (Float, Float) {
  return (1, 1)
}

// CHECK-LABEL: sil @dfoo

//===----------------------------------------------------------------------===//
// Indirect returns
//===----------------------------------------------------------------------===//

@_silgen_name("foo_indir_ret")
@differentiable(reverse, adjoint: dfoo_indir_ret(_:_:_:_:))
public func foo_indir_ret<T>(_ x: Float, _ y: T) -> T {
  return y
}

// CHECK-LABEL: sil [reverse_differentiable source 0 wrt 0, 1 primal @foo_indir_ret adjoint @dfoo_indir_ret primitive] @foo_indir_ret : $@convention(thin) <T> (Float, @in_guaranteed T) -> @out T {
// CHECK: bb0(%0 : @trivial $*T, %1 : @trivial $Float, %2 : @trivial $*T):

@_silgen_name("dfoo_indir_ret")
public func dfoo_indir_ret<T>(_ x: Float, _ y: T, _ partial: T, _ seed: T) -> (Float, T) {
  return (x, y)
}

// CHECK-LABEL: sil @dfoo_indir_ret : $@convention(thin) <T> (Float, @in_guaranteed T, @in_guaranteed T, @in_guaranteed T) -> (Float, @out T) {
// CHECK: bb0(%0 : @trivial $*T, %1 : @trivial $Float, %2 : @trivial $*T, %3 : @trivial $*T, %4 : @trivial $*T):

//===----------------------------------------------------------------------===//
// Flattened types
//===----------------------------------------------------------------------===//

@_silgen_name("foo_tuple")
@differentiable(reverse, adjoint: dfoo_tuple(_:_:partial:seed:))
public func foo_tuple(_ x: ((Float, (Float, Float)), Float, ((Float))), _ y: Float) -> Float {
  return 1
}

// CHECK-LABEL: sil [reverse_differentiable source 0 wrt 0, 1, 2, 3, 4, 5 primal @foo_tuple adjoint @dfoo_tuple primitive] @foo_tuple : $@convention(thin) (Float, Float, Float, Float, Float, Float) -> Float

@_silgen_name("dfoo_tuple")
public func dfoo_tuple(_ x: ((Float, (Float, Float)), Float, ((Float))), _ y: Float, partial: Float, seed: Float) -> (((Float, (Float, Float)), Float, ((Float))), Float) {
  return (((1, (1, 1)), 1, ((1))), 1)
}

// CHECK-LABEL: sil @dfoo_tuple : $@convention(thin) (Float, Float, Float, Float, Float, Float, Float, Float) -> (Float, Float, Float, Float, Float, Float)

@_silgen_name("no_prim_or_adj")
@differentiable(reverse) // ok!
public func no_prim_or_adj(_ x: Float) -> Float {
  return x * x
}

// CHECK-LABEL: sil [reverse_differentiable source 0 wrt 0] @no_prim_or_adj : $@convention(thin) (Float) -> Float
