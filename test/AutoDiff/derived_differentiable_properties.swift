// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil -verify %s

public struct Foo : Differentiable {
  public var a: Float
}

// CHECK-AST-LABEL: public struct Foo : Differentiable {
// CHECK-AST:   @sil_stored @differentiable(wrt: (self))
// CHECK-AST:   public var a: Float { get set }
// CHECK-AST:   @_fieldwiseProductSpace struct AllDifferentiableVariables
// CHECK-AST:     @_fieldwiseProductSpace typealias AllDifferentiableVariables = Foo.AllDifferentiableVariables
// CHECK-AST:     @_fieldwiseProductSpace typealias TangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:     @_fieldwiseProductSpace typealias CotangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:   @_fieldwiseProductSpace typealias TangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:   @_fieldwiseProductSpace typealias CotangentVector = Foo.AllDifferentiableVariables

// CHECK-SILGEN-LABEL: // Foo.a.getter
// CHECK-SILGEN: sil [transparent] [serialized] [differentiable source 0 wrt 0] @$s33derived_differentiable_properties3FooV1aSfvg : $@convention(method) (Foo) -> Float

struct AdditiveTangentIsSelf : AdditiveArithmetic, Differentiable {
  var a: Float
}
let _: @autodiff (AdditiveTangentIsSelf) -> Float = { x in
  x.a + x.a
}

// CHECK-AST-LABEL: struct AdditiveTangentIsSelf : AdditiveArithmetic, Differentiable {
// CHECK-AST-NOT:     @differentiable
// CHECK-AST:         @_fieldwiseProductSpace typealias TangentVector = AdditiveTangentIsSelf
// CHECK-AST:         @_fieldwiseProductSpace typealias CotangentVector = AdditiveTangentIsSelf
// FIXME:             `typealias AllDifferentiableVariables` should have `@_fieldwiseProductSpace`.
// CHECK-AST:         typealias AllDifferentiableVariables = AdditiveTangentIsSelf

struct TestNoDerivative : Differentiable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: struct TestNoDerivative : Differentiable {
// CHECK-AST:         @sil_stored var w: Float { get set }
// CHECK-AST:         @sil_stored @noDerivative var technicallyDifferentiable: Float { get set }
// CHECK-AST:         @_fieldwiseProductSpace struct AllDifferentiableVariables : Differentiable, AdditiveArithmetic, VectorNumeric
// CHECK-AST:           @_fieldwiseProductSpace typealias AllDifferentiableVariables = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:           @_fieldwiseProductSpace typealias TangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:           @_fieldwiseProductSpace typealias CotangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:         @_fieldwiseProductSpace typealias TangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:         @_fieldwiseProductSpace typealias CotangentVector = TestNoDerivative.AllDifferentiableVariables

struct TestKeyPathIterable : Differentiable, KeyPathIterable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: struct TestKeyPathIterable : Differentiable, KeyPathIterable {
// CHECK-AST:         @sil_stored var w: Float { get set }
// CHECK-AST:         @sil_stored @noDerivative var technicallyDifferentiable: Float { get set }
// CHECK-AST:         @_fieldwiseProductSpace struct AllDifferentiableVariables : Differentiable, AdditiveArithmetic, KeyPathIterable, VectorNumeric
// CHECK-AST:           @_fieldwiseProductSpace typealias AllDifferentiableVariables = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:           @_fieldwiseProductSpace typealias TangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:           @_fieldwiseProductSpace typealias CotangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:         @_fieldwiseProductSpace typealias TangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:         @_fieldwiseProductSpace typealias CotangentVector = TestKeyPathIterable.AllDifferentiableVariables
