// RUN: %target-swift-frontend -emit-silgen %s | %target-sil-opt | %FileCheck %s

// Test SIL differentiability witness SIL generation.

// Test public non-generic function.
// SIL differentiability witness:
// - Has public linkage (implicit).
// - Has no `where` clause.

public func foo(_ x: Float) -> Float { x }

@derivative(of: foo)
public func foo_jvp(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
  (x, { $0 })
}

@derivative(of: foo)
public func foo_vjp(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x, { $0 })
}

// CHECK-LABEL: // differentiability witness for foo(_:)
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] @$s36sil_differentiability_witness_silgen3fooyS2fF : $@convention(thin) (Float) -> Float {
// CHECK-NEXT:   jvp: @AD__$s36sil_differentiability_witness_silgen3fooyS2fF__jvp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-NEXT:   vjp: @AD__$s36sil_differentiability_witness_silgen3fooyS2fF__vjp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-NEXT: }

// Test internal non-generic function.
// SIL differentiability witness:
// - Has hidden linkage.
// - Has no `where` clause.
// - Has only VJP.

func bar<T>(_ x: Float, _ y: T) -> Float { x }

@derivative(of: bar)
public func bar_jvp<T>(_ x: Float, _ y: T) -> (value: Float, differential: (Float) -> Float) {
  (x, { $0 })
}

// CHECK-LABEL: // differentiability witness for bar<A>(_:_:)
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0] [results 0] <τ_0_0> @$s36sil_differentiability_witness_silgen3baryS2f_xtlF : $@convention(thin) <T> (Float, @in_guaranteed T) -> Float {
// CHECK-NEXT:   jvp: @AD__$s36sil_differentiability_witness_silgen3baryS2f_xtlF__jvp_src_0_wrt_0_l : $@convention(thin) <τ_0_0> (Float, @in_guaranteed τ_0_0) -> (Float, @owned @callee_guaranteed (Float) -> Float)
// CHECK-NEXT: }

// Test internal generic function.
// SIL differentiability witness:
// - Has hidden linkage.
// - Has `where` clause.

func generic<T>(_ x: T, _ y: Float) -> T { x }

@derivative(of: generic)
func generic_jvp<T: Differentiable>(_ x: T, _ y: Float) -> (
  value: T, differential: (T.TangentVector, Float) -> T.TangentVector
) {
  (x, { dx, dy in dx })
}

@derivative(of: generic)
func generic_vjp<T: Differentiable>(_ x: T, _ y: Float) -> (
  value: T, pullback: (T.TangentVector) -> (T.TangentVector, Float)
) {
  (x, { ($0, .zero) })
}

// CHECK-LABEL: // differentiability witness for generic<A>(_:_:)
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0 1] [results 0] <τ_0_0 where τ_0_0 : Differentiable> @$s36sil_differentiability_witness_silgen7genericyxx_SftlF : $@convention(thin) <T> (@in_guaranteed T, Float) -> @out T {
// CHECK-NEXT:   jvp: @AD__$s36sil_differentiability_witness_silgen7genericyxx_SftlF__jvp_src_0_wrt_0_1_s14DifferentiableRzl : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable> (@in_guaranteed τ_0_0, Float) -> (@out τ_0_0, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0, Float) -> @out τ_0_1 for <τ_0_0.TangentVector, τ_0_0.TangentVector>)
// CHECK-NEXT:   vjp: @AD__$s36sil_differentiability_witness_silgen7genericyxx_SftlF__vjp_src_0_wrt_0_1_s14DifferentiableRzl : $@convention(thin) <τ_0_0 where τ_0_0 : Differentiable> (@in_guaranteed τ_0_0, Float) -> (@out τ_0_0, @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> (@out τ_0_1, Float) for <τ_0_0.TangentVector, τ_0_0.TangentVector>)
// CHECK-NEXT: }

public struct Foo: Differentiable {
  public var x: Float

// CHECK-LABEL: // differentiability witness for Foo.x.getter
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] @$s36sil_differentiability_witness_silgen3FooV1xSfvg : $@convention(method) (Foo) -> Float {
// CHECK-NEXT: }

  @differentiable
  public init(_ x: Float) {
    self.x = x
  }

// CHECK-LABEL: // differentiability witness for Foo.init(_:)
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] @$s36sil_differentiability_witness_silgen3FooVyACSfcfC : $@convention(method) (Float, @thin Foo.Type) -> Foo {
// CHECK-NEXT: }

  @differentiable
  public func method() -> Float {
    x
  }

// CHECK-LABEL: // differentiability witness for Foo.method()
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] @$s36sil_differentiability_witness_silgen3FooV6methodSfyF : $@convention(method) (Foo) -> Float {
// CHECK-NEXT: }

  @differentiable
  public var computedProperty: Float {
    x
  }

// CHECK-LABEL: // differentiability witness for Foo.computedProperty.getter
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] @$s36sil_differentiability_witness_silgen3FooV16computedPropertySfvg : $@convention(method) (Foo) -> Float {
// CHECK-NEXT: }

  @differentiable
  public subscript() -> Float {
    x
  }

// CHECK-LABEL: // differentiability witness for Foo.subscript.getter
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] @$s36sil_differentiability_witness_silgen3FooVSfycig : $@convention(method) (Foo) -> Float {
// CHECK-NEXT: }
}

// Test function that is differentiable wrt subset of its parameters:
// - wrt x: explicit @differentiable attribute, with no custom derivative specified
// - wrt y: explicit @differentiable attribute, with custom derivative specified
// - wrt x, y: custom deriviative specified, with no explicit @differentiable attribute
// Has a tuple argument to verify that indices are correctly lowered to SIL.

@differentiable(wrt: x)
public func wrt_subset(_ tup: (Int, Int), _ x: Float, _ y: Float) -> Float {
  return 0
}

@derivative(of: wrt_subset, wrt: y)
public func wrt_subset_jvp_wrt_y(_ tup: (Int, Int), _ x: Float, _ y: Float) -> (value: Float, differential: (Float) -> Float) {
  return (0, { $0 })
}

@derivative(of: wrt_subset, wrt: y)
public func wrt_subset_vjp_wrt_y(_ tup: (Int, Int), _ x: Float, _ y: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (0, { $0 })
}

@derivative(of: wrt_subset)
public func wrt_subset_jvp_wrt_x_y(_ tup: (Int, Int), _ x: Float, _ y: Float) -> (value: Float, differential: (Float, Float) -> Float) {
  return (0, { $0 + $1 })
}

@derivative(of: wrt_subset)
public func wrt_subset_vjp_wrt_x_y(_ tup: (Int, Int), _ x: Float, _ y: Float) -> (value: Float, pullback: (Float) -> (Float, Float)) {
  return (0, { ($0, $0) })
}

// CHECK-LABEL: // differentiability witness for wrt_subset(_:_:_:)
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 2] [results 0] @$s36sil_differentiability_witness_silgen10wrt_subsetySfSi_Sit_S2ftF : $@convention(thin) (Int, Int, Float, Float) -> Float {
// CHECK-NEXT: }

// CHECK-LABEL: // differentiability witness for wrt_subset(_:_:_:)
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 3] [results 0] @$s36sil_differentiability_witness_silgen10wrt_subsetySfSi_Sit_S2ftF : $@convention(thin) (Int, Int, Float, Float) -> Float {
// CHECK-NEXT:   jvp:
// CHECK-NEXT:   vjp:
// CHECK-NEXT: }

// CHECK-LABEL: // differentiability witness for wrt_subset(_:_:_:)
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 2 3] [results 0] @$s36sil_differentiability_witness_silgen10wrt_subsetySfSi_Sit_S2ftF : $@convention(thin) (Int, Int, Float, Float) -> Float {
// CHECK-NEXT:   jvp:
// CHECK-NEXT:   vjp:
// CHECK-NEXT: }

// Test original function with `@differentiable` and `@derivative` attributes.

protocol P1: Differentiable {}
extension P1 {
  @differentiable // derivative generic signature: none
  func foo() -> Float { 1 }
}
extension P1 {
  @derivative(of: foo) // derivative generic signature: `<P1 where Self: P1>`
  func vjpFoo() -> (value: Float, pullback: (Float) -> (TangentVector)) {
    fatalError()
  }
}

// CHECK-LABEL: // differentiability witness for P1.foo()
// CHECK-NEXT: sil_differentiability_witness hidden [parameters 0] [results 0] <τ_0_0 where τ_0_0 : P1> @$s36sil_differentiability_witness_silgen2P1PAAE3fooSfyF : $@convention(method) <Self where Self : P1> (@in_guaranteed Self) -> Float {
// CHECK-NEXT:   vjp: @AD__$s36sil_differentiability_witness_silgen2P1PAAE3fooSfyF__vjp_src_0_wrt_0_36sil_differentiability_witness_silgen2P1Rzl : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> (Float, @owned @callee_guaranteed @substituted <τ_0_0> (Float) -> @out τ_0_0 for <τ_0_0.TangentVector>)
// CHECK-NEXT: }

// Test custom derivatives of functions with generic signatures and `@differentiable` attributes.

@differentiable
@_silgen_name("genericWithDiffAttr")
public func genericWithDiffAttr<T: Differentiable>(_ x: T) -> T { fatalError() }

@derivative(of: genericWithDiffAttr)
public func vjpGenericWithDiffAttr<T: Differentiable>(_ x: T)
  -> (value: T, pullback: (T.TangentVector) -> T.TangentVector)
{
  fatalError()
}

// CHECK-LABEL: // differentiability witness for genericWithDiffAttr
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] <τ_0_0 where τ_0_0 : Differentiable> @genericWithDiffAttr : $@convention(thin) <T where T : Differentiable> (@in_guaranteed T) -> @out T {
// CHECK-NEXT:   vjp
// CHECK-NEXT: }

// CHECK-NOT: // differentiability witness for genericWithDiffAttr

@differentiable(where T: Differentiable)
@_silgen_name("genericWithConstrainedDifferentiable")
public func genericWithConstrainedDifferentiable<T>(_ x: T) -> T { fatalError() }

@derivative(of: genericWithConstrainedDifferentiable)
public func vjpGenericWithConstrainedDifferentiable<T: Differentiable>(_ x: T)
  -> (value: T, pullback: (T.TangentVector) -> T.TangentVector)
{
  fatalError()
}

// CHECK-LABEL: // differentiability witness for genericWithConstrainedDifferentiable
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] <τ_0_0 where τ_0_0 : Differentiable> @genericWithConstrainedDifferentiable : $@convention(thin) <T> (@in_guaranteed T) -> @out T {
// CHECK-NEXT:   vjp
// CHECK-NEXT: }

// CHECK-NOT: // differentiability witness for genericWithConstrainedDifferentiable

public extension Differentiable {
  @differentiable
  @_silgen_name("protocolExtensionWithDiffAttr")
  func protocolExtensionWithDiffAttr() -> Self { self }

  @derivative(of: protocolExtensionWithDiffAttr)
  func protocolExtensionWithDiffAttr() -> (value: Self, pullback: (TangentVector) -> TangentVector) {
    fatalError("unimplemented")
  }
}

// CHECK-LABEL: // differentiability witness for protocolExtensionWithDiffAttr
// CHECK-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] <τ_0_0 where τ_0_0 : Differentiable> @protocolExtensionWithDiffAttr : $@convention(method) <Self where Self : Differentiable> (@in_guaranteed Self) -> @out Self {
// CHECK-NEXT:   vjp
// CHECK-NEXT: }

// CHECK-NOT: // differentiability witness for protocolExtensionWithDiffAttr
