// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil -verify %s

public struct Foo : Differentiable {
  public var a: Float
}

// CHECK-AST-LABEL: @_fieldwiseDifferentiable public struct Foo : Differentiable {
// CHECK-AST:   @_hasStorage @differentiable(wrt: (self))
// CHECK-AST:   public var a: Float { get set }
// CHECK-AST:   @_fieldwiseDifferentiable struct AllDifferentiableVariables
// CHECK-AST:     typealias AllDifferentiableVariables = Foo.AllDifferentiableVariables
// CHECK-AST:     typealias TangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:     typealias CotangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:   typealias TangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:   typealias CotangentVector = Foo.AllDifferentiableVariables

// CHECK-SILGEN-LABEL: // Foo.a.getter
// CHECK-SILGEN: sil [transparent] [serialized] [differentiable source 0 wrt 0] @$s33derived_differentiable_properties3FooV1aSfvg : $@convention(method) (Foo) -> Float

struct AdditiveTangentIsSelf : AdditiveArithmetic, Differentiable {
  var a: Float
}
let _: @differentiable (AdditiveTangentIsSelf) -> Float = { x in
  x.a + x.a
}

// CHECK-AST-LABEL: @_fieldwiseDifferentiable struct AdditiveTangentIsSelf : AdditiveArithmetic, Differentiable {
// CHECK-AST-NOT:     @differentiable
// CHECK-AST:         typealias TangentVector = AdditiveTangentIsSelf
// CHECK-AST:         typealias CotangentVector = AdditiveTangentIsSelf
// CHECK-AST:         typealias AllDifferentiableVariables = AdditiveTangentIsSelf

struct TestNoDerivative : Differentiable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: @_fieldwiseDifferentiable struct TestNoDerivative : Differentiable {
// CHECK-AST:         @_hasStorage var w: Float { get set }
// CHECK-AST:         @_hasStorage @noDerivative var technicallyDifferentiable: Float { get set }
// CHECK-AST:         @_fieldwiseDifferentiable struct AllDifferentiableVariables : Differentiable, AdditiveArithmetic, VectorNumeric
// CHECK-AST:           typealias AllDifferentiableVariables = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:           typealias TangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:           typealias CotangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:         typealias TangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:         typealias CotangentVector = TestNoDerivative.AllDifferentiableVariables

struct TestKeyPathIterable : Differentiable, KeyPathIterable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: @_fieldwiseDifferentiable struct TestKeyPathIterable : Differentiable, KeyPathIterable {
// CHECK-AST:         @_hasStorage var w: Float { get set }
// CHECK-AST:         @_hasStorage @noDerivative var technicallyDifferentiable: Float { get set }
// CHECK-AST:         @_fieldwiseDifferentiable struct AllDifferentiableVariables : Differentiable, AdditiveArithmetic, KeyPathIterable, VectorNumeric
// CHECK-AST:           typealias AllDifferentiableVariables = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:           typealias TangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:           typealias CotangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:         typealias TangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:         typealias CotangentVector = TestKeyPathIterable.AllDifferentiableVariables
