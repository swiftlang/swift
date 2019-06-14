// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s --check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s --check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil -verify %s

public struct Foo : Differentiable {
  public var a: Float
}

// CHECK-AST-LABEL: public struct Foo : Differentiable {
// CHECK-AST:   @differentiable
// CHECK-AST:   public var a: Float
// CHECK-AST:   internal init(a: Float)
// CHECK-AST:   public struct AllDifferentiableVariables
// CHECK-AST:     public typealias AllDifferentiableVariables = Foo.AllDifferentiableVariables
// CHECK-AST:     public typealias TangentVector = Foo.AllDifferentiableVariables
// CHECK-AST:   public typealias TangentVector = Foo.AllDifferentiableVariables

// CHECK-SILGEN-LABEL: // Foo.a.getter
// CHECK-SILGEN-NEXT: sil [transparent] [serialized] [differentiable source 0 wrt 0] [ossa] @$s33derived_differentiable_properties3FooV1aSfvg : $@convention(method) (Foo) -> Float

struct AdditiveTangentIsSelf : AdditiveArithmetic, Differentiable {
  var a: Float
}
let _: @differentiable (AdditiveTangentIsSelf) -> Float = { x in
  x.a + x.a
}

// CHECK-AST-LABEL: internal struct AdditiveTangentIsSelf : AdditiveArithmetic, Differentiable {
// CHECK-AST:         internal var a: Float
// CHECK-AST:         internal init(a: Float)
// CHECK-AST:         internal typealias TangentVector = AdditiveTangentIsSelf
// CHECK-AST:         internal typealias AllDifferentiableVariables = AdditiveTangentIsSelf

struct TestNoDerivative : Differentiable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: internal struct TestNoDerivative : Differentiable {
// CHECK-AST:         var w: Float
// CHECK-AST:         @noDerivative internal var technicallyDifferentiable: Float
// CHECK-AST:         internal init(w: Float, technicallyDifferentiable: Float)
// CHECK-AST:         internal struct AllDifferentiableVariables : Differentiable, AdditiveArithmetic, VectorProtocol
// CHECK-AST:           internal typealias AllDifferentiableVariables = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:           internal typealias TangentVector = TestNoDerivative.AllDifferentiableVariables
// CHECK-AST:         internal typealias TangentVector = TestNoDerivative.AllDifferentiableVariables

struct TestKeyPathIterable : Differentiable, KeyPathIterable {
  var w: Float
  @noDerivative var technicallyDifferentiable: Float
}

// CHECK-AST-LABEL: internal struct TestKeyPathIterable : Differentiable, KeyPathIterable {
// CHECK-AST:         var w: Float
// CHECK-AST:         @noDerivative internal var technicallyDifferentiable: Float
// CHECK-AST:         internal init(w: Float, technicallyDifferentiable: Float)
// CHECK-AST:         internal struct AllDifferentiableVariables : Differentiable, AdditiveArithmetic, KeyPathIterable, VectorProtocol
// CHECK-AST:           internal typealias AllDifferentiableVariables = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:           internal typealias TangentVector = TestKeyPathIterable.AllDifferentiableVariables
// CHECK-AST:         internal typealias TangentVector = TestKeyPathIterable.AllDifferentiableVariables

struct GenericTanMember<T : Differentiable> : Differentiable, AdditiveArithmetic {
  var x: T.TangentVector
}

// TODO(TF-316): Revisit after `Differentiable` derived conformances behavior is standardized.
// `AllDifferentiableVariables` and `TangentVector` structs need not both be synthesized.

// CHECK-AST-LABEL: internal struct GenericTanMember<T> : Differentiable, AdditiveArithmetic where T : Differentiable
// CHECK-AST:   internal var x: T.TangentVector
// CHECK-AST:   internal init(x: T.TangentVector)
// CHECK-AST:   internal typealias TangentVector = GenericTanMember<T>
// CHECK-AST:   internal typealias AllDifferentiableVariables = GenericTanMember<T>
// CHECK-AST:   internal static var zero: GenericTanMember<T> { get }
// CHECK-AST:   internal static func + (lhs: GenericTanMember<T>, rhs: GenericTanMember<T>) -> GenericTanMember<T>
// CHECK-AST:   internal static func - (lhs: GenericTanMember<T>, rhs: GenericTanMember<T>) -> GenericTanMember<T>
// CHECK-AST:   @_implements(Equatable, ==(_:_:)) internal static func __derived_struct_equals(_ a: GenericTanMember<T>, _ b: GenericTanMember<T>) -> Bool

public struct ConditionallyDifferentiable<T> {
  public var x: T
}
extension ConditionallyDifferentiable : Differentiable where T : Differentiable {}

// CHECK-AST-LABEL: public struct ConditionallyDifferentiable<T> {
// CHECK-AST:         @differentiable(wrt: self where T : Differentiable)
// CHECK-AST:         public var x: T
// CHECK-AST:         internal init(x: T)
// CHECK-AST:       }
