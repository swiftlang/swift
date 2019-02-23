// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s --check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil -verify %s

public struct Foo : Differentiable {
  public var a: Float
}

// CHECK-AST-LABEL: @_fieldwiseDifferentiable public struct Foo : Differentiable {
// CHECK-AST:   @differentiable(wrt: (self))
// CHECK-AST:   public var a: Float
// CHECK-AST:   internal init(a: Float)
// CHECK-AST:   @_fieldwiseDifferentiable public struct AllDifferentiableVariables
// CHECK-AST:     public typealias AllDifferentiableVariables = Foo.AllDifferentiableVariables
// CHECK-AST:     public typealias TangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:     public typealias CotangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:   public typealias TangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:   public typealias CotangentVector = Foo.AllDifferentiableVariables

// CHECK-SILGEN-LABEL: // Foo.a.getter
// CHECK-SILGEN-NEXT: sil [transparent] [serialized] [differentiable source 0 wrt 0] @$s33derived_differentiable_properties3FooV1aSfvg : $@convention(method) (Foo) -> Float

struct AdditiveTangentIsSelf : AdditiveArithmetic, Differentiable {
  var a: Float
}
let _: @differentiable (AdditiveTangentIsSelf) -> Float = { x in
  x.a + x.a
}

// CHECK-AST-LABEL: @_fieldwiseDifferentiable internal struct AdditiveTangentIsSelf : AdditiveArithmetic, Differentiable {
// CHECK-AST:         internal var a: Float
// CHECK-AST:         internal init(a: Float)
// CHECK-AST:         internal typealias TangentVector = AdditiveTangentIsSelf
// CHECK-AST:         internal typealias CotangentVector = AdditiveTangentIsSelf
// CHECK-AST:         internal typealias AllDifferentiableVariables = AdditiveTangentIsSelf

struct TestNoDerivative : Differentiable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: @_fieldwiseDifferentiable internal struct TestNoDerivative : Differentiable {
// CHECK-AST:         var w: Float
// CHECK-AST:         @noDerivative internal var technicallyDifferentiable: Float
// CHECK-AST:         internal init(w: Float, technicallyDifferentiable: Float)
// CHECK-AST:         @_fieldwiseDifferentiable internal struct AllDifferentiableVariables : Differentiable, AdditiveArithmetic, VectorNumeric
// CHECK-AST:           internal typealias AllDifferentiableVariables = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:           internal typealias TangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:           internal typealias CotangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:         internal typealias TangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:         internal typealias CotangentVector = TestNoDerivative.AllDifferentiableVariables

struct TestKeyPathIterable : Differentiable, KeyPathIterable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: @_fieldwiseDifferentiable internal struct TestKeyPathIterable : Differentiable, KeyPathIterable {
// CHECK-AST:         var w: Float
// CHECK-AST:         @noDerivative internal var technicallyDifferentiable: Float
// CHECK-AST:         internal init(w: Float, technicallyDifferentiable: Float)
// CHECK-AST:         @_fieldwiseDifferentiable internal struct AllDifferentiableVariables : Differentiable, AdditiveArithmetic, KeyPathIterable, VectorNumeric
// CHECK-AST:           internal typealias AllDifferentiableVariables = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:           internal typealias TangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:           internal typealias CotangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:         internal typealias TangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:         internal typealias CotangentVector = TestKeyPathIterable.AllDifferentiableVariables
