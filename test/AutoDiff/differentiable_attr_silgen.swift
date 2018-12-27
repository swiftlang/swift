// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -emit-silgen -verify %s | %FileCheck %s

//===----------------------------------------------------------------------===//
// Normal types
//===----------------------------------------------------------------------===//

@_silgen_name("foo")
@differentiable(adjoint: dfoo)
public func foo(_ x: Float, _ y: Float) -> Float {
  return 1
}

// CHECK-LABEL: sil [differentiable source 0 wrt 0, 1 primal @foo adjoint @dfoo primitive] @foo

@_silgen_name("dfoo")
public func dfoo(_ seed: Float, partial: Float, _ x: Float, _ y: Float) -> (Float, Float) {
  return (1, 1)
}

// CHECK-LABEL: sil @dfoo

//===----------------------------------------------------------------------===//
// Indirect returns
//===----------------------------------------------------------------------===//

@_silgen_name("foo_indir_ret")
@differentiable(adjoint: dfoo_indir_ret)
public func foo_indir_ret<T: Differentiable>(_ x: Float, _ y: T) -> T {
  return y
}

// CHECK-LABEL: sil [differentiable source 0 wrt 0, 1 primal @foo_indir_ret adjoint @dfoo_indir_ret primitive] @foo_indir_ret : $@convention(thin) <T where T : Differentiable> (Float, @in_guaranteed T) -> @out T {
// CHECK: bb0(%0 : @trivial $*T, %1 : @trivial $Float, %2 : @trivial $*T):

@_silgen_name("dfoo_indir_ret")
public func dfoo_indir_ret<T: Differentiable>(_ seed: T.CotangentVector, _ partial: T, _ x: Float, _ y: T) -> (Float, T.CotangentVector) {
  return (x, seed)
}

// CHECK-LABEL: sil @dfoo_indir_ret : $@convention(thin) <T where T : Differentiable> (@in_guaranteed T.CotangentVector, @in_guaranteed T, Float, @in_guaranteed T) -> (Float, @out T.CotangentVector) {
// CHECK: bb0(%0 : @trivial $*T.CotangentVector, %1 : @trivial $*T.CotangentVector, %2 : @trivial $*T, %3 : @trivial $Float, %4 : @trivial $*T):

//===----------------------------------------------------------------------===//
// Flattened types
//===----------------------------------------------------------------------===//

@_silgen_name("foo_tuple")
@differentiable(adjoint: dfoo_tuple)
public func foo_tuple(_ x: ((Float, (Float, Float)), Float, ((Float))), _ y: Float) -> Float {
  return 1
}

// CHECK-LABEL: sil [differentiable source 0 wrt 0, 1, 2, 3, 4, 5 primal @foo_tuple adjoint @dfoo_tuple primitive] @foo_tuple : $@convention(thin) (Float, Float, Float, Float, Float, Float) -> Float

@_silgen_name("dfoo_tuple")
public func dfoo_tuple(_ seed: Float, partial: Float, _ x: ((Float, (Float, Float)), Float, ((Float))), _ y: Float) -> (((Float, (Float, Float)), Float, ((Float))), Float) {
  return (((1, (1, 1)), 1, ((1))), 1)
}

// CHECK-LABEL: sil @dfoo_tuple : $@convention(thin) (Float, Float, Float, Float, Float, Float, Float, Float) -> (Float, Float, Float, Float, Float, Float)

@_silgen_name("no_prim_or_adj")
@differentiable() // ok!
public func no_prim_or_adj(_ x: Float) -> Float {
  return x * x
}

// CHECK-LABEL: sil [differentiable source 0 wrt 0] @no_prim_or_adj : $@convention(thin) (Float) -> Float

//===----------------------------------------------------------------------===//
// JVP
//===----------------------------------------------------------------------===//

@_silgen_name("hasjvp")
@differentiable(jvp: dhasjvp)
public func hasjvp(_ x: Float, _ y: Float) -> Float {
  return 1
}

// CHECK-LABEL: sil [differentiable source 0 wrt 0, 1 jvp @dhasjvp] @hasjvp

@_silgen_name("dhasjvp")
public func dhasjvp(_ x: Float, _ y: Float) -> (Float, (Float, Float) -> Float) {
  return (1, { _, _ in 1 })
}

// CHECK-LABEL: sil @dhasjvp

//===----------------------------------------------------------------------===//
// VJP
//===----------------------------------------------------------------------===//

@_silgen_name("hasvjp")
@differentiable(vjp: dhasvjp)
public func hasvjp(_ x: Float, _ y: Float) -> Float {
  return 1
}

// CHECK-LABEL: sil [differentiable source 0 wrt 0, 1 vjp @dhasvjp] @hasvjp

@_silgen_name("dhasvjp")
public func dhasvjp(_ x: Float, _ y: Float) -> (Float, (Float) -> (Float, Float)) {
  return (1, { _ in (1, 1) })
}

// CHECK-LABEL: sil @dhasvjp
