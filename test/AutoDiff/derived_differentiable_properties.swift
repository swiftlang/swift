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
// CHECK-AST:   typealias TangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:   typealias CotangentVector = Foo.AllDifferentiableVariables

// CHECK-SILGEN-LABEL: // Foo.a.getter
// CHECK-SILGEN: sil [transparent] [serialized] [differentiable source 0 wrt 0 primitive] @$s33derived_differentiable_properties3FooV1aSfvg : $@convention(method) (Foo) -> Float

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
